
type:
  JsonResponse:
    header:
        Content-Type: 'application/json'
    body: Json

  HtmlResponse:
    header:
        Content-Type: 'text/html'
    body: Html

endpoint:
    "/hello":
        GET:
            response:
                JsonResponse:
                    body:
                        Json:
                            message: String 
            

endpoint."/fizzbuzz".GET:
    request:
        param:
            arg: int
    response:
        JsonResponse.body.JSON.answer: 
            Union:
                - Int
                - String
                - 
                    error: String

endpoint "/index.html" GET:
  response:
    body: 
        Html.body.Selector.'#header'.innerText:
            'Hello World'
