#[cfg(test)]
mod test {
    const TEST_A2L: &str = r###"
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
    /begin COMPU_METHOD compumethod
      ""
      RAT_FUNC
      "%6.3"
      ""
      COEFFS 0 1 0 0 0 1
      REF_UNIT abc
    /end COMPU_METHOD
  /end MODULE
/end PROJECT"###;

    #[test]
    fn round_trip() {
        let mut log_msgs = Vec::<String>::new();
        let a2lfile = a2lfile::load_from_string(TEST_A2L, None, &mut log_msgs, false).unwrap();
        let text = a2lfile.write_to_string();
        println!("input:\n{}\noutput:\n{}\n", TEST_A2L, text);

        assert_eq!(TEST_A2L, text);
    }
}
