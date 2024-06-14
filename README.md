# kaleidoscope.hs
My dialect of kaleidoscope, statically typed with type inference.

language example:
```
struct Bar {
  a: i32,
  b: float, // just double
  c: char,  // just u8
}

fn bar(x: i32, y: i32) -> i32 {
  var a = x + y; // typeof a : i32
  return a;
}
```
