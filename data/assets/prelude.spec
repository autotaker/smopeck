type Request = Object {
    header: RequestHeader,
    body: RequestBody
}

type RequestHeader = Object 
type RequestBody = Object 


type ResponseHeader = Object 
type ResponseBody = String
type JsonResponse = Response {
    header: ResponseHeader {
        'Content-Type' : String [ . = 'application/json' ]
    },
    body: Object
}
type Response = Object {
    header: ResponseHeader,
    body: ResponseBody
}

type Endpoint = Object {
    request: Request,
    response: JsonResponse
}
