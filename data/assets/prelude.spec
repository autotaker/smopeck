type Request = Object {
    header: RequestHeader,
    body: RequestBody
}
type JsonRequest = Request {
    header: RequestHeader {
        'Content-Type' : String [ . = 'application/json' ]
    },
    body: Object
}

type RequestHeader = Object 
type RequestBody = Object | Null


type ResponseHeader = Object 
type ResponseBody = String
type JsonResponseHeader = ResponseHeader {
    'Content-Type' : String [ . = 'application/json' ]
}
type JsonResponse = Response {
    header: JsonResponseHeader,
    body: Object
}
type Response = Object {
    header: ResponseHeader,
    body: ResponseBody
}

type Endpoint = Object {
    parameter : Parameter,
    request: Request,
    response: JsonResponse
}

type Parameter = Object {
    path: Object,
    query: Object
}
