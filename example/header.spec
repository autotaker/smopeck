type Request = Object {
    header: RequestHeader,
    body: RequestBody
}

type RequestHeader = Object 
type RequestBody = Object 

type ResponseStatus = Object {
    code: Int [ . >= 100, . < 600 ],
    reason: String
}

type ResponseHeader = Object 
type ResponseBody = String
type JsonResponse = Response {
    header: ResponseHeader {
        contentType : String [ . = 'application/json' ]
    },
    body: Object
}
type Response = Object {
    status: ResponseStatus,
    header: ResponseHeader,
    body: ResponseBody
}

type Endpoint = Object {
    request: Request,
    response: JsonResponse
}

type Main = Endpoint