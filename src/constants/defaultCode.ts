export const defaultCodeSnippets: Record<string, string> = {
  javascript: `// JavaScript (Node.js 12.14.0)
console.log("Hi from Quick Compile!");`,

  assembly: `; Assembly (NASM 2.14.02)
section .data
    msg db "Hi from Quick Compile!", 0

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
echo "Hi from Quick Compile!"`,

  basic: `10 PRINT "Hi from Quick Compile!"
20 END`,

  c: `#include <stdio.h>
int main() {
    printf("Hi from Quick Compile!\\n");
    return 0;
}`,

  cpp: `#include <iostream>
int main() {
    std::cout << "Hi from Quick Compile!" << std::endl;
    return 0;
}`,

  clojure: `(println "Hi from Quick Compile!")`,

  csharp: `using System;
class Program {
    static void Main() {
        Console.WriteLine("Hi from Quick Compile!");
    }
}`,

  cobol: `IDENTIFICATION DIVISION.
PROGRAM-ID. HelloWorld.
PROCEDURE DIVISION.
    DISPLAY "Hi from Quick Compile!".
    STOP RUN.`,

  lisp: `(format t "Hi from Quick Compile!~%")`,

  d: `import std.stdio;
void main() {
    writeln("Hi from Quick Compile!");
}`,

  elixir: `IO.puts "Hi from Quick Compile!"`,

  erlang: `-module(hello).
-export([hello_world/0]).
hello_world() ->
    io:format("Hi from Quick Compile!~n").`,

  exe: `# Executable placeholder - no code to show`,

  fsharp: `printfn "Hi from Quick Compile!"`,

  fortran: `program hello
    print *, "Hi from Quick Compile!"
end program hello`,

  go: `package main
import "fmt"
func main() {
    fmt.Println("Hi from Quick Compile!")
}`,

  groovy: `println 'Hi from Quick Compile!'`,

  haskell: `main = putStrLn "Hi from Quick Compile!"`,

  java: `public class Main {
    public static void main(String[] args) {
        System.out.println("Hi from Quick Compile!");
    }
}`,

  kotlin: `fun main() {
    println("Hi from Quick Compile!")
}`,

  lua: `print("Hi from Quick Compile!")`,

  objectivec: `#import <Foundation/Foundation.h>
int main() {
    @autoreleasepool {
        NSLog(@"Hi from Quick Compile!");
    }
    return 0;
}`,

  ocaml: `print_endline "Hi from Quick Compile!";;`,

  octave: `disp('Hi from Quick Compile!')`,

  pascal: `program HelloWorld;
begin
  writeln('Hi from Quick Compile!');
end.`,

  perl: `print "Hi from Quick Compile!\\n";`,

  php: `<?php
echo "Hi from Quick Compile!\\n";
?>`,

  text: `// Plain text - no code`,

  prolog: `:- initialization(main).
main :- write('Hi from Quick Compile!'), nl, halt.`,

  python: `print("Hi from Quick Compile!")`,

  r: `print("Hi from Quick Compile!")`,

  ruby: `puts "Hi from Quick Compile!"`,

  rust: `fn main() {
    println!("Hi from Quick Compile!");
}`,

  scala: `object Main extends App {
    println("Hi from Quick Compile!")
}`,

  sql: `-- SQL example
SELECT 'Hi from Quick Compile!';`,

  swift: `print("Hi from Quick Compile!")`,

  typescript: `console.log("Hi from Quick Compile!");`,
};
