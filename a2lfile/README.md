# `a2lfile`

[![Github Actions](https://github.com/DanielT/a2lfile/actions/workflows/test.yml/badge.svg)](https://github.com/DanielT/a2lfile/actions)
[![codecov](https://codecov.io/gh/DanielT/a2lfile/branch/main/graph/badge.svg)](https://codecov.io/gh/DanielT/a2lfile)
[![license](https://img.shields.io/badge/license-Apache--2.0_OR_MIT-blue)](#license)

`a2lfile` is a library that allows you to read, modify and write a2l files.

## Features

- full support for files using A2L version 1.7.1
- it is fast
- the layout and format of the input file is preserved. The intention is that after reading, modifying and then writing a file the resulting diff should be minimal
- easy access to application-specific data inside of IF_DATA blocks is provided through a macro that generates code based on an A2ML specification

## What is an a2l file

A2l files are commonly used during the development and testing of automotive ECUs.
The consumer of the a2l file typically performs online calibration over a protocol such as [XCP](https://en.wikipedia.org/wiki/XCP_(protocol)) and/or offline tuning by generating flashable parameter sets.

If you have never seen an a2l file then you are unlikely to need this library.

## Tools

The program [a2ltool](https://github.com/DanielT/a2ltool) is based on this library.

## Documentation

Add this to your `Cargo.toml`:

```toml
[dependencies]
a2lfile = "2.0"
```

A simple program based on the `a2lfile` library might look like this:

```rust
use a2lfile::*;

fn main() {
    let input_filename = &std::ffi::OsString::from("example.a2l");
    let mut logmsgs = Vec::<A2LError>::new();
    let mut a2l_file = a2lfile::load(
        input_filename,
        None,
        &mut logmsgs,
        false
    ).expect("could not load the file");
    for log_msg in logmsgs {
        println!("warning while loading the file: {}", log_msg);
    }

    // perform a consistency check
    let mut logmsgs = Vec::<String>::new();
    a2l_file.check(&mut logmsgs);
    for log_msg in logmsgs {
        println!("warning during consistency check: {}", log_msg);
    }

    for measurement in &a2l_file.project.module[0].measurement {
        // do something with the MEASUREMENT objects in the file
        println!("MEASUREMENT: {:#?}", measurement);
    }

    // create a new CHARACTERISTIC object
    let new_characteristic = Characteristic::new(
        "my_name".to_string(),
        "my extended description".to_string(),
        CharacteristicType::Value,
        0x12345678,
        "something.RECORD_LAYOUT".to_string(),
        0.0,
        "NO_COMPU_METHOD".to_string(),
        0.0,
        100.0
    );
    a2l_file.project.module[0].characteristic.push(new_characteristic);


    // update the sorting to find a suitable insertion point for the new characteristic - by default it will be placed at the end
    a2l_file.sort_new_items();

    // write the modified file
    a2l_file.write(
        &std::ffi::OsString::from("example_output.txt"),
        Some("modified by the demo program")
    ).expect("failed to write output");
}

```

## License

Licensed under either of

- Apache License, Version 2.0 ([`LICENSE-APACHE`](./LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([`LICENSE-MIT`](./LICENSE-MIT) or <http://opensource.org/licenses/MIT>)
