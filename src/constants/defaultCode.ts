export const defaultCodeSnippets: Record<string, string> = {
  javascript: `// JavaScript (Node.js 12.14.0)
console.log("Hello, world!");`,

  assembly: `; Assembly (NASM 2.14.02)
section .data
    msg db "Hello, world!", 0

section .text
    global _start
_start:
    ; write msg to stdout
    mov edx, 13
    mov ecx, msg
    mov ebx, 1
    mov eax, 4
    int 0x80

    ; exit
    mov eax, 1
    int 0x80`,

  bash: `#!/bin/bash
echo "Hello, world!"`,

  basic: `10 PRINT "Hello, world!"
20 END`,

  c: `#include <stdio.h>
int main() {
    printf("Hello, world!\\n");
    return 0;
}`,

  cpp: `#include <iostream>
int main() {
    std::cout << "Hello, world!" << std::endl;
    return 0;
}`,

  clojure: `(println "Hello, world!")`,

  csharp: `using System;
class Program {
    static void Main() {
        Console.WriteLine("Hello, world!");
    }
}`,

  cobol: `IDENTIFICATION DIVISION.
PROGRAM-ID. HelloWorld.
PROCEDURE DIVISION.
    DISPLAY "Hello, world!".
    STOP RUN.`,

  lisp: `(format t "Hello, world!~%")`,

  d: `import std.stdio;
void main() {
    writeln("Hello, world!");
}`,

  elixir: `IO.puts "Hello, world!"`,

  erlang: `-module(hello).
-export([hello_world/0]).
hello_world() ->
    io:format("Hello, world!~n").`,

  exe: `# Executable placeholder - no code to show`,

  fsharp: `printfn "Hello, world!"`,

  fortran: `program hello
    print *, "Hello, world!"
end program hello`,

  go: `package main
import "fmt"
func main() {
    fmt.Println("Hello, world!")
}`,

  groovy: `println 'Hello, world!'`,

  haskell: `main = putStrLn "Hello, world!"`,

  java: `public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }
}`,

  kotlin: `fun main() {
    println("Hello, world!")
}`,

  lua: `print("Hello, world!")`,

  objectivec: `#import <Foundation/Foundation.h>
int main() {
    @autoreleasepool {
        NSLog(@"Hello, world!");
    }
    return 0;
}`,

  ocaml: `print_endline "Hello, world!";;`,

  octave: `disp('Hello, world!')`,

  pascal: `program HelloWorld;
begin
  writeln('Hello, world!');
end.`,

  perl: `print "Hello, world!\\n";`,

  php: `<?php
echo "Hello, world!\\n";
?>`,

  text: `// Plain text - no code`,

  prolog: `:- initialization(main).
main :- write('Hello, world!'), nl, halt.`,

  python: `print("Hello, world!")`,

  r: `print("Hello, world!")`,

  ruby: `puts "Hello, world!"`,

  rust: `fn main() {
    println!("Hello, world!");
}`,

  scala: `object Main extends App {
    println("Hello, world!")
}`,

  sql: `-- SQL example
SELECT 'Hello, world!';`,

  swift: `print("Hello, world!")`,

  typescript: `console.log("Hello, world!");`,
};
