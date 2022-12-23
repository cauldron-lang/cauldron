use crate::eval::object::Object;

pub fn print(str: String) -> Object {
    println!("{:?}", str);

    Object::Void
}
