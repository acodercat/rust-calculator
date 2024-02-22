mod calculator;

use std::io::{self, Write};
use calculator::process_expression;

fn main() {
    println!("Welcome to the calculator for Toptal!");
    println!("Enter an expression, or type 'exit' to quit.");

    loop {
        print!("> ");
        // Flush stdout to ensure the prompt appears immediately
        io::stdout().flush().expect("Failed to flush stdout.");

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();
                if input.eq_ignore_ascii_case("exit") {
                    break; // Exit the loop if the input is "exit"
                }

                // Process the expression entered by the user
                match process_expression(input) {
                    Ok(result) => {
                        println!("{}", result)
                    }, // If your function already prints the result, you don't need to do anything here.
                    Err(e) => println!("Error: {:?}", e), // Print error messages
                }
            },
            Err(error) => println!("Error reading input: {}", error),
        }
    }

    println!("Thank you for using the calculator!");
}
