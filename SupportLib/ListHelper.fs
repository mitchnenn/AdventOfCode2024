namespace SupportLib

module ListHelper =
    let appendReverse (input: 'a list) : 'a list =
        input @ (List.rev input)

