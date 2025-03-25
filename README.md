# kaleidoscope.hs

My dialect of kaleidoscope, statically typed with type inference.

language example:

```
// ADTs
data Bar = Bar1 Int Bool | Bar2 String;

// auto-curry: bar :: Bar -> i32 -> i32
fn bar(x: Bar, y: i32) -> i32 {
  match x {
    (Bar1 i b) => i + 2,
    _ => 1,
  }
}

// adhoc polymorphism, pass a dictionary under the hood
// Show :: Type -> Constraint
protocol Show self {
  fn show(self) -> String;
}

// parametric polymorphism
// foo :: forall T. Show T => T -> i32
fn foo<T: Show>(t: T) -> i32 {
  length . show $ t    // function application without parens
}
```
