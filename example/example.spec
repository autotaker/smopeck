
type JsonResponse = HttpResponse {
    header: { Content-Type: "application/json" }
    body: Json
}

type HtmlResponse = HttpResponse {
    header: { Content-Type: "text/html" }
    body: Html
}

type Json = Number | String | Bool | Object | Array | Null

type Int = Number [ . isInt ]
type StringArray = Array {
    element : Bool,
    length : Int[ . < 10]
}

type 


type Message<T> = "hello $T" -- abbreviation for String[. = "hello $T"]

type Digits = r"\d+" -- abbreviation for String[ . ~= r"\d+" ]

type Positive = Int[. > 0]

type EqPair = Object{
    fst: Int,
    snd: Int
}[ .fst = .snd ]


"/hello" GET = Endpoint{
    response.JsonResponse#body.Json#message: String 
        {-  Abbreviation for
            response : JsonResponse{
                body: Json{message: String}
            }
        -}
}

endpoint "/echo" POST {
    request: HttpRequest{ body : String }
    response: JsonResponse{ body: Json{message: "Echo request.HttpRequest#body" }}
}
                     
endpoint "/fizzbuzz" GET {
    request: HttpRequest{ 
        parameter: URLParamter{
            arg: Int
        }
    }
    response: JsonResponse {
        body: Json {
            answer: Int | "fizz" | "buzz" | "fizzbuzz" | { error: String }
        }
    }
}

endpoint "/index.html" GET {
    response: HtmlResponse {
        body: Html{
            head: HtmlHeader {
                    title: "Index Page"
                }
            body: Selector<"#message"> {
                    innerText: 'Hello World!'
                } & Selector<".button"> {
                    tag: 'Button'
                }
        }
    }
}
