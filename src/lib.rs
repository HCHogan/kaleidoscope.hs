mod ast;
mod codegen;
mod token;

use hs_bindgen::*;

#[hs_bindgen(add :: CInt -> CInt -> IO (CInt))]
pub fn add(left: i32, right: i32) -> i32 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
