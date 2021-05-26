# `a2lfile`

`a2lfile` is a library that allows you to read, modify and write a2l files.

## Features

- full support for files using A2L version 1.61
- nearly complete support for version the newest standard 1.71. It handles existing files, but unusual elements might cause problems.
- it is fast
- the layout and formatting of the input file is preserved. The intention is that afer reading, modifying and then writing a file the resulting diff should be minimal
- easy access to application-specific data inside of IF_DATA blocks is provided through a macro that generates code based on an A2ML specification

## What is an a2l file

A2l files are commonly used during the development and testing of automotive ECUs.
If you have never seen an a2l file then you are unlikely to need this library.

## Documentation

Add this to your `Cargo.toml`:

```toml
[dependencies]
a2lfile = "0.8.0"
```

A simple program based on the `a2lfile` library might look like this:

```rust
use a2lfile::*;

struct A2lLogger {
    log: Vec<String>
}

impl a2lfile::Logger for A2lLogger {
    fn log_message(&mut self, msg: String) {
        self.log.push(msg);
    }
}

fn main() {
    let input_filename = "example.a2l";
    let mut logger = A2lLogger { log: Vec::new() };
    let mut a2l_file = a2lfile::load(
        input_filename,
        None,
        &mut logger,
        false
    ).expect("could not load the file");
    for log_msg in logger.log {
        println!("warning while loading the file: {}", log_msg);
    }

    // perform a consistency check
    let mut logger = A2lLogger { log: Vec::new() };
    a2l_file.check(&mut logger);
    for log_msg in logger.log {
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
        "example_output.txt",
        Some("modified by the demo program")
    ).expect("failed to write output");
}

```

## License

Licensed under either of

  * Apache License, Version 2.0 ([`LICENSE-APACHE`](./LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
  * MIT license ([`LICENSE-MIT`](./LICENSE-MIT) or http://opensource.org/licenses/MIT)
