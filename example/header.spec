type Request = Object {
    header: RequestHeader,
    body: RequestBody
}

type RequestHeader = Object 
type RequestBody = Object 

type ResponseStatus = Object {
    code: Int,
    reason: String
}

type ResponseHeader = Object 
type ResponseBody = JsonResponse | HtmlResponse
type HtmlReponse = String
type JsonResponse = Object

type Response = Object {
    status: ResponseStatus,
    header: ResponseHeader,
    body: ResponseBody
}

type Endpoint = Object {
    request: Request,
    response: Response
}

type Main = Endpoint