module Domain

[<Measure>]
type piece

[<Measure>]
type cent

type Item = Item of char
type SomeOfPricing = { Items: (Item * int<piece>) list; Price: int<cent> }
type AnyOfPricing = { ChooseFrom: Item list; Quantity: int<piece>; Price: int<cent> }
type Pricing = SomeOf of SomeOfPricing | AnyOf of AnyOfPricing
type PriceState = { Rest: (Item * int<piece>) list; TotalPrice: int<cent> }