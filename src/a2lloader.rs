use std::fs::File;
use std::io::Read;
use encoding::all::ISO_8859_1;
use encoding::Encoding;


pub fn load(filename: &str) -> Result<String, String> {
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(error) => return Err(format!("could not open file: {}", error))
    };
    let filedata = read_data(&mut file)?;
    let utf8data = decode_raw_bytes(filedata);

    let data = if utf8data.len() > 2 && utf8data.chars().next().unwrap() == '\u{feff}' {
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
    if read_result.is_err() {
        return Err(format!("failed to read from file: {}", read_result.err().unwrap()));
    }
    Ok(buffer)
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
            let mut filedata_unicode: Vec<char> =  Vec::with_capacity(filedata.len()/4 as usize);
            for i in 0..(filedata.len()/4) {
                let charbytes: [u8; 4] = [filedata[i*4], filedata[i*4+1], filedata[i*4+2], filedata[i*4+3]];
                let nextchar = std::char::from_u32(u32conversion.unwrap()(charbytes));
                if nextchar.is_none() {
                    conversion_failed = true;
                    break;
                }
                filedata_unicode.push(nextchar.unwrap());
            }
            if conversion_failed == false {
                return filedata_unicode.into_iter().collect();
            }
        }
    }

    /* check UTF-16
     * Big endian bom is 0xfe 0xff. Without BOM, the fist character should be 0x00 0x??
     * little endian bom is 0xff 0xfe. Without BOM, the fist character should be 0x?? 0x00 */
    if (filedata.len() % 2 == 0) && (filedata.len() > 1) {
        let u16conversion: Option<fn([u8; 2]) -> u16> = 
            if ((filedata[0] == 0) && (filedata[1] != 0)) || ((filedata[0] == 0xfe && filedata[1] == 0xff)) {
                Some(u16::from_be_bytes) 
            } else if ((filedata[0] != 0) && (filedata[1] == 0)) || ((filedata[0] == 0xff && filedata[1] == 0xfe)) {  
                Some(u16::from_le_bytes) 
            } else { 
                None
            };
        if u16conversion.is_some() {
            let mut filedata_u16 = Vec::with_capacity(filedata.len() / 2);
            for i in 0..(filedata.len()/2) {
                let u16bytes: [u8; 2] = [filedata[i*2], filedata[i*2+1]];
                filedata_u16.push(u16conversion.unwrap()(u16bytes));
            }
            let converted_u16 = String::from_utf16(&filedata_u16);
            if converted_u16.is_ok() {
                return converted_u16.unwrap();
            }
        }
    }

    /* try to handle the data as pure utf-8 */
    let converted = String::from_utf8(filedata.clone());
    if converted.is_ok() {
        return converted.unwrap();
    }

    /* try to handle the data as ISO8859-1 */
    let converted = ISO_8859_1.decode(&filedata, encoding::DecoderTrap::Strict);
    if converted.is_ok() {
        return converted.unwrap().to_owned();
    }

    /* fallback: decode the data as UTF-8 while discarding invalid bytes.
     * Generally, everything outside of comments and descriptive text should be pure ascii anyway... */
    return String::from_utf8_lossy(&filedata).into_owned();
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

