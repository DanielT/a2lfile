use std::ffi::OsStr;
use std::fs::File;
use std::io::Read;


pub fn load(filename: &OsStr) -> Result<String, String> {
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(error) => return Err(format!("Error while loading {}: {}\n", filename.to_string_lossy(), error))
    };

    let filedata = read_data(&mut file)?;
    let utf8data = decode_raw_bytes(filedata);

    let data = if utf8data.len() > 2 && utf8data.starts_with('\u{feff}') {
        // it has a BOM, strip that off here
        String::from(&utf8data[3..])
    } else {
        utf8data
    };

    Ok(data)
}


fn read_data(file: &mut File) -> Result<Vec<u8>, String> {
    let filesize = file.metadata().unwrap().len();
    let mut buffer = Vec::with_capacity(filesize as usize);
    let read_result = file.read_to_end(&mut buffer);
    match read_result {
        Ok(_) => Ok(buffer),
        Err(err) => Err(format!("Error: failed to read from file: {}", err))
    }
}


fn decode_raw_bytes(filedata: Vec<u8>) -> String {
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
        if u32conversion.is_some() {
            let mut conversion_failed = false;
            let mut filedata_unicode: Vec<char> =  Vec::with_capacity(filedata.len()/4_usize);
            for i in 0..(filedata.len()/4) {
                let charbytes: [u8; 4] = [filedata[i*4], filedata[i*4+1], filedata[i*4+2], filedata[i*4+3]];
                let nextchar = std::char::from_u32(u32conversion.unwrap()(charbytes));
                if nextchar.is_none() {
                    conversion_failed = true;
                    break;
                }
                filedata_unicode.push(nextchar.unwrap());
            }
            if !conversion_failed {
                return filedata_unicode.into_iter().collect();
            }
        }
    }

    /* check UTF-16
     * Big endian bom is 0xfe 0xff. Without BOM, the fist character should be 0x00 0x??
     * little endian bom is 0xff 0xfe. Without BOM, the fist character should be 0x?? 0x00 */
    if (filedata.len() % 2 == 0) && (filedata.len() > 1) {
        let u16conversion: Option<fn([u8; 2]) -> u16> = 
            if ((filedata[0] == 0) && (filedata[1] != 0)) || (filedata[0] == 0xfe && filedata[1] == 0xff) {
                Some(u16::from_be_bytes) 
            } else if ((filedata[0] != 0) && (filedata[1] == 0)) || (filedata[0] == 0xff && filedata[1] == 0xfe) {  
                Some(u16::from_le_bytes) 
            } else { 
                None
            };
        if let Some(conversion) = u16conversion {
            let mut filedata_u16 = Vec::with_capacity(filedata.len() / 2);
            for i in 0..(filedata.len()/2) {
                let u16bytes: [u8; 2] = [filedata[i*2], filedata[i*2+1]];
                filedata_u16.push(conversion(u16bytes));
            }
            if let Ok(converted_u16) = String::from_utf16(&filedata_u16) {
                return converted_u16;
            }
        }
    }

    /* try to handle the data as pure utf-8 */
    if let Ok(converted) = String::from_utf8(filedata.clone()) {
        return converted;
    }

    /* handle the data as ISO8859-1. This always succeeds, because every sequence of bytes can be a latin-1 string */
    let mut outstr = String::with_capacity(filedata.len());
    filedata.iter().for_each(|ch| outstr.push(*ch as char));
    outstr
}







/*************************************************************************************************/



#[test]
fn decode_raw_bytes_u32() {
    // big endian
    let data : Vec<u8> = vec![0, 0, 0, 65, 0, 0, 0, 66];
    assert_eq!(decode_raw_bytes(data), String::from("AB"));
    // big endian, with BOM
    let data : Vec<u8> = vec![0, 0, 0xfe, 0xff, 0, 0, 0, 65, 0, 0, 0, 66];
    assert_eq!(decode_raw_bytes(data), String::from("\u{feff}AB"));
    // little endian
    let data : Vec<u8> = vec![65, 0, 0, 0, 66, 0, 0, 0];
    assert_eq!(decode_raw_bytes(data), String::from("AB"));
    // little endian, with BOM
    let data : Vec<u8> = vec![0xff, 0xfe, 0, 0, 65, 0, 0, 0, 66, 0, 0, 0];
    assert_eq!(decode_raw_bytes(data), String::from("\u{feff}AB"));
    // mixed endian (error)
    let data : Vec<u8> = vec![0, 0, 0, 65, 66, 0, 0, 00];
    assert_ne!(decode_raw_bytes(data), String::from("AB"));    
}

#[test]
fn decode_raw_bytes_u16() {
    // big endian
    let data : Vec<u8> = vec![65, 0, 66, 0, 65, 0, 66, 0];
    assert_eq!(decode_raw_bytes(data), String::from("ABAB"));
    // big endian, with BOM
    let data : Vec<u8> = vec![0xff, 0xfe, 65, 0, 66, 0, 65, 0, 66, 0];
    assert_eq!(decode_raw_bytes(data), String::from("\u{feff}ABAB"));
    // little endian
    let data : Vec<u8> = vec![00, 65, 0, 66, 0, 65, 0, 66];
    assert_eq!(decode_raw_bytes(data), String::from("ABAB"));
    // little endian, with BOM
    let data : Vec<u8> = vec![0xfe, 0xff, 00, 65, 0, 66, 0, 65, 0, 66];
    assert_eq!(decode_raw_bytes(data), String::from("\u{feff}ABAB"));
    // mixed endian
    let data : Vec<u8> = vec![00, 65, 0, 66, 65, 0, 66, 00];
    assert_ne!(decode_raw_bytes(data), String::from("ABAB"));
}

#[test]
fn decode_raw_bytes_u8() {
    let data : Vec<u8> = vec![65, 66, 65, 66];
    assert_eq!(decode_raw_bytes(data), String::from("ABAB"));
    let data : Vec<u8> = vec![239, 187, 191, 65, 66];
    assert_eq!(decode_raw_bytes(data), String::from("\u{feff}AB"));
}

