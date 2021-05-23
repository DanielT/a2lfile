#[cfg(test)]
mod test {
    const TEST_A2L: &str = r###"/* written by a2ltool */
ASAP2_VERSION 1 61
/begin PROJECT SOMETHING ""

  /begin MODULE CPP ""
    /begin MOD_COMMON ""
      BYTE_ORDER MSB_LAST
      ALIGNMENT_BYTE 1
      ALIGNMENT_WORD 1
      ALIGNMENT_LONG 1
      ALIGNMENT_INT64 1
      ALIGNMENT_FLOAT32_IEEE 1
      ALIGNMENT_FLOAT64_IEEE 1
    /end MOD_COMMON
  /end MODULE
/end PROJECT"###;

    struct A2lLogger {
        log: Vec<String>
    }
    
    impl a2lfile::Logger for A2lLogger {
        fn log_message(&mut self, msg: String) {
            self.log.push(msg);
        }
    }

    #[test]
    fn round_trip() {
        let mut logger = A2lLogger { log: Vec::new() };
        let a2lfile = a2lfile::load_from_string(TEST_A2L, None, &mut logger, false).unwrap();
        let text = a2lfile.write_to_string();
        println!("input:\n{}\noutput:\n{}\n", TEST_A2L, text);

        assert_eq!(TEST_A2L, text);
    }
}