use crate::A2lError;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::Read;
use std::path::{Path, MAIN_SEPARATOR};

pub(crate) fn make_include_filename(incname: &str, base_filename: &OsStr) -> OsString {
    let normalized_incname: String = incname
        .replace('\\', MAIN_SEPARATOR.to_string().as_str())
        .replace('/', MAIN_SEPARATOR.to_string().as_str());

    let inc_path = Path::new(&normalized_incname);

    if inc_path.is_absolute() {
        return OsString::from(inc_path);
    }

    let base = Path::new(base_filename);

    // If base has a parent directory, resolve relative path
    if let Some(basedir) = base.parent() {
        let joined = basedir.join(inc_path);
        if joined.exists() {
            return OsString::from(joined);
        }
    }

    OsString::from(incname)
}

pub fn load(path: &Path) -> Result<String, A2lError> {
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(error) => {
            return Err(A2lError::FileOpenError {
                filename: path.to_path_buf(),
                ioerror: error,
            });
        }
    };

    let filedata = read_data(&mut file, path)?;
    let utf8data = decode_raw_bytes(&filedata);

    let data = if utf8data.len() > 2 && utf8data.starts_with('\u{feff}') {
        // it has a BOM, strip that off here
        String::from(&utf8data[3..])
    } else {
        utf8data
    };

    Ok(data)
}

fn read_data(file: &mut File, path: &Path) -> Result<Vec<u8>, A2lError> {
    let filesize = match file.metadata() {
        Ok(metadata) => metadata.len(),
        Err(err) => {
            return Err(A2lError::FileReadError {
                filename: path.to_path_buf(),
                ioerror: err,
            })
        }
    };
    let bufsize = usize::try_from(filesize).unwrap_or(usize::MAX); // filesize > 4GB on 32bit systems - is it sane ?!?
    let mut buffer = Vec::with_capacity(bufsize);
    let read_result = file.read_to_end(&mut buffer);
    match read_result {
        Ok(_) => Ok(buffer),
        Err(err) => Err(A2lError::FileReadError {
            filename: path.to_path_buf(),
            ioerror: err,
        }),
    }
}

fn decode_raw_bytes(filedata: &[u8]) -> String {
    /* an a2l file must have either a BOM or a character from the basic ASCII set as the first character in the file
    we can use this to guess the encoding, because we expect to see nul-bytes in the first character if UTF-16 or UTF-32 is used. */

    /* check UTF-32.
     * Big endian format: the filedata should be 0x00 0x00 0xFE 0xFF if it is a BOM, or 00 00 00 xx otherwise.
     * Little endian format: The filedata should be 0xFF 0xFE 0x00 0x00 if it is a BOM, or xx 00 00 00 otherwise.*/
    if (filedata.len() % 4 == 0) && (filedata.len() > 3) {
        let u32conversion: Option<fn([u8; 4]) -> u32> =
            if (filedata[0] == 0) && (filedata[1] == 0) && (filedata[3] != 0) {
                Some(u32::from_be_bytes)
            } else if (filedata[0] != 0) && (filedata[2] == 0) && (filedata[3] == 0) {
                Some(u32::from_le_bytes)
            } else {
                None
            };
        if let Some(conversion) = u32conversion {
            let mut conversion_failed = false;
            let mut filedata_unicode: Vec<char> = Vec::with_capacity(filedata.len() / 4_usize);
            for i in 0..(filedata.len() / 4) {
                let charbytes: [u8; 4] = [
                    filedata[i * 4],
                    filedata[i * 4 + 1],
                    filedata[i * 4 + 2],
                    filedata[i * 4 + 3],
                ];
                match std::char::from_u32(conversion(charbytes)) {
                    Some(nextchar) => filedata_unicode.push(nextchar),
                    None => {
                        conversion_failed = true;
                        break;
                    }
                }
            }
            if !conversion_failed {
                return filedata_unicode.into_iter().collect();
            }
        }
    }

    /* check UTF-16
     * Big endian bom is 0xfe 0xff. Without BOM, the first character should be 0x00 0x??
     * little endian bom is 0xff 0xfe. Without BOM, the first character should be 0x?? 0x00 */
    if (filedata.len() % 2 == 0) && (filedata.len() > 1) {
        let u16conversion: Option<fn([u8; 2]) -> u16> = if ((filedata[0] == 0)
            && (filedata[1] != 0))
            || (filedata[0] == 0xfe && filedata[1] == 0xff)
        {
            Some(u16::from_be_bytes)
        } else if ((filedata[0] != 0) && (filedata[1] == 0))
            || (filedata[0] == 0xff && filedata[1] == 0xfe)
        {
            Some(u16::from_le_bytes)
        } else {
            None
        };
        if let Some(conversion) = u16conversion {
            let mut filedata_u16 = Vec::with_capacity(filedata.len() / 2);
            for i in 0..(filedata.len() / 2) {
                let u16bytes: [u8; 2] = [filedata[i * 2], filedata[i * 2 + 1]];
                filedata_u16.push(conversion(u16bytes));
            }
            if let Ok(converted_u16) = String::from_utf16(&filedata_u16) {
                return converted_u16;
            }
        }
    }

    /* try to handle the data as pure utf-8 */
    if let Ok(converted) = String::from_utf8(filedata.to_vec()) {
        return converted;
    }

    /* handle the data as ISO8859-1. This always succeeds, because every sequence of bytes can be a latin-1 string */
    let mut outstr = String::with_capacity(filedata.len());
    filedata.iter().for_each(|ch| outstr.push(*ch as char));
    outstr
}

/*************************************************************************************************/

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn load_nonexistent_file() {
        let result = load(Path::new("file/does/not/exist"));
        assert!(result.is_err());
    }

    #[test]
    fn decode_raw_bytes_u32() {
        // big endian
        let data: Vec<u8> = vec![0, 0, 0, 65, 0, 0, 0, 66];
        assert_eq!(decode_raw_bytes(&data), String::from("AB"));
        // big endian, with BOM
        let data: Vec<u8> = vec![0, 0, 0xfe, 0xff, 0, 0, 0, 65, 0, 0, 0, 66];
        assert_eq!(decode_raw_bytes(&data), String::from("\u{feff}AB"));
        // little endian
        let data: Vec<u8> = vec![65, 0, 0, 0, 66, 0, 0, 0];
        assert_eq!(decode_raw_bytes(&data), String::from("AB"));
        // little endian, with BOM
        let data: Vec<u8> = vec![0xff, 0xfe, 0, 0, 65, 0, 0, 0, 66, 0, 0, 0];
        assert_eq!(decode_raw_bytes(&data), String::from("\u{feff}AB"));
        // mixed endian (error)
        let data: Vec<u8> = vec![0, 0, 0, 65, 66, 0, 0, 00];
        assert_ne!(decode_raw_bytes(&data), String::from("AB"));
    }

    #[test]
    fn decode_raw_bytes_u16() {
        // big endian
        let data: Vec<u8> = vec![65, 0, 66, 0, 65, 0, 66, 0];
        assert_eq!(decode_raw_bytes(&data), String::from("ABAB"));
        // big endian, with BOM
        let data: Vec<u8> = vec![0xff, 0xfe, 65, 0, 66, 0, 65, 0, 66, 0];
        assert_eq!(decode_raw_bytes(&data), String::from("\u{feff}ABAB"));
        // little endian
        let data: Vec<u8> = vec![00, 65, 0, 66, 0, 65, 0, 66];
        assert_eq!(decode_raw_bytes(&data), String::from("ABAB"));
        // little endian, with BOM
        let data: Vec<u8> = vec![0xfe, 0xff, 00, 65, 0, 66, 0, 65, 0, 66];
        assert_eq!(decode_raw_bytes(&data), String::from("\u{feff}ABAB"));
        // mixed endian
        let data: Vec<u8> = vec![00, 65, 0, 66, 65, 0, 66, 00];
        assert_ne!(decode_raw_bytes(&data), String::from("ABAB"));
    }

    #[test]
    fn decode_raw_bytes_u8() {
        let data: Vec<u8> = vec![65, 66, 65, 66];
        assert_eq!(decode_raw_bytes(&data), String::from("ABAB"));
        let data: Vec<u8> = vec![239, 187, 191, 65, 66];
        assert_eq!(decode_raw_bytes(&data), String::from("\u{feff}AB"));
    }

    #[test]
    fn decode_decode_raw_bytes_ascii() {
        let data: Vec<u8> = vec![0xa9]; // "©"
        assert_eq!(decode_raw_bytes(&data), String::from("\u{00a9}"));
    }

    #[test]
    fn included_files() {
        use std::path::{Path, MAIN_SEPARATOR};
        let dir = tempdir().unwrap();

        // base file at <tempdir>/base
        let base_filename = dir.path().join("base");
        let mut basefile = std::fs::File::create_new(&base_filename).unwrap();
        basefile.write_all(br#"/include "abc/include1""#).unwrap();

        // include file 1 at <tempdir>/abc/include1
        let subdir = dir.path().join("abc");
        let inc1name = subdir.join("include1");
        std::fs::create_dir(&subdir).unwrap();
        let mut incfile1 = std::fs::File::create_new(&inc1name).unwrap();
        incfile1.write_all(br#"/include "def/include2""#).unwrap();

        // include file 2 at <tempdir>/abc/def/include2
        let subdir2 = subdir.join("def");
        std::fs::create_dir(&subdir2).unwrap();
        let _incfile2 = std::fs::File::create_new(subdir2.join("include2")).unwrap();

        // verify include 1
        let out = make_include_filename(r#"abc/include1"#, base_filename.as_os_str())
            .into_string()
            .unwrap();
        // canonicalize both out and expected - this fixes issues with "/" and "\" so that the test passes on windows and linux
        let out_path = Path::new(&out).canonicalize().unwrap();
        let expected = dir.path().join("abc/include1").canonicalize().unwrap();
        assert_eq!(out_path.to_string_lossy(), expected.to_string_lossy());

        // verify include 2
        let out = make_include_filename(r#"def/include2"#, inc1name.as_os_str())
            .into_string()
            .unwrap();
        // canonicalize both out and expected - this fixes issues with "/" and "\" so that the test passes on windows and linux
        let out_path = Path::new(&out).canonicalize().unwrap();
        let expected = subdir.join("def/include2").canonicalize().unwrap();
        assert_eq!(out_path.to_string_lossy(), expected.to_string_lossy());

        // verify if cross platform paths work
        let (incname, expected_subdir) = if cfg!(windows) {
            // unix-style input path on Windows
            ("abc/include1", format!("abc{}include1", MAIN_SEPARATOR))
        } else {
            // Windows-style input path on Unix
            (r"abc\include1", format!("abc{}include1", MAIN_SEPARATOR))
        };
        let out = make_include_filename(incname, base_filename.as_os_str())
            .into_string()
            .unwrap();
        let out_path = Path::new(&out)
            .canonicalize()
            .expect("Output path should be resolvable");
        let expected_path = dir.path().join(expected_subdir);
        let expected = expected_path
            .canonicalize()
            .expect("Expected path should be resolvable");
        assert_eq!(
            out_path.to_string_lossy(),
            expected.to_string_lossy(),
            "Normalized output does not match"
        );
    }
}
