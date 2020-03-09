endpoint "/" GET {
    response: JsonResponse {
        body: Object {
            message: String [ .= 'hello world!'],
            id: Int [ . > 0 ],
            feed: Array {
                length: Int [ . = 10 ],
                get(i): Object {
                    'message': String,
                    'id': Int [ . > 0]
                }
            }
        }
    }
}

endpoint "/hello" GET {
    response: JsonResponse {
        body: String [ . = 'hello world!' ]
    }
}

endpoint "/param" GET {
    request: Request {
        param: Object {
            name: String
        }
    },
    response: JsonResponse {
        body: Object {
            message: String [ . = 'hello: ' + request.param.name ],
            secret: String
        }     
    }
}