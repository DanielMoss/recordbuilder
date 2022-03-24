# Record builder

### The problem
I'm working with files containing data like this:
```text
{{
name
|fieldA = a string
|fieldB = an int
|fieldC = something else
}}
```
This object corresponds to an entity. The type of the entity can be determined from the first line (`name`). Each field is then separated by the `|` character, and has a unique name and expected value type.

However there's a couple of problems:
- there's a large variety of entity types
- fields are unordered
- fields are nullable
- the input data is unsanitised
  - the formatting of values may be invalid
  - fields may be incorrectly named
  - required fields may not be defined
  - the files contain _a lot_ of other data - not just entity definitions

Additionally, fields can be individually versioned by suffixing them with an index, like this:
```text
{{
name
|fieldA1 = a string
|fieldA2 = another string
|fieldB = an int
}}
```
This is equivalent to two entities:
```text
{{
name
|fieldA = a string
|fieldB = an int
}}
{{
name
|fieldA = another string
|fieldB = an int
}}
```

There's a substantial amount of data that I need to parse regularly, so performance matters.

### The general approach
I've modelled entities using a standard ADT approach. For example, the entity corresponding to the definitions above might look like
```scala
trait Entity

final case class Name(a: String, b: Int, c: Option[Something])
```

I'm trying out [parboiled](https://github.com/sirthias/parboiled2) for parsing the files. I'd like the output of my parsers to be a `List[Entity]`. For parsing an individual entity, my rough idea is to place an appropriate builder onto the stack once I know the entity type, and then progressively update that builder on the stack as I parse the fields. Once all fields have been parsed, I'll then try to build the entity.

I'd like to parse fields into the appropriate types as I iterate through them. I could roll with something like
```scala
final case class NameBuilder(
  maybeA: Option[String],
  maybeB: Option[Int],
  maybeC: Option[Something]
)
```
but this introduces a lot of boilerplate. It not only means having mirroring entities and entity builders, but also having to map parsers to the appropriate builder field. Given the number of entity types I'm dealing with, implementing this would be incredibly boring.

Instead, I'd like to have a more strongly typed `Map` to store the fields and their typed values in, akin to [shapeless](https://github.com/milessabin/shapeless)' `HMap`. At the time I was working on this, I wasn't aware that `HMap` existed. I was, however, aware of shapeless' `Record`s.

I ended up with something I called `RecordBuilder`, which solves the problems I was having with storing and retrieving typed field data. This project documents a much-streamlined pathway to the implementation I ended up with, which I'm hoping will help solidify some of the things I've learnt from this kind of programming.
