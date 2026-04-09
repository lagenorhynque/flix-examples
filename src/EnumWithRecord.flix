mod EnumWithRecord {
    enum Person({ name = String, age = Int32 })

    instance ToString[Person] {
        pub def toString(p: Person): String =
            let Person.Person({ name, age }) = p;
            "Person(name = ${name}, age = ${age})"
    }

    pub def example(): Unit \ IO =
        let p = Person.Person({ name = "Alice", age = 30 });
        println("Person: ${p}")
}
