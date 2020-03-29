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
    parameter: Parameter {
        query : Object {
            name : String,
            id: Int [ . > 0, . < 100],
            action : String [ . = 'add'] | String [ . = 'delete' ]
        }
    },
    response: JsonResponse {
        body: Object {
            message: String [ . = 'hello: ' + parameter.query.name ],
            secret: String,
            id: Int [ . = parameter.query.id ]
        }     
    }
}

endpoint "/search" GET {
    parameter: Parameter {
        query : Object {
            pageSize : Int [ . > 0, . <= 100]
        }
    },
    response: JsonResponse {
        body: Array {
            length: Int [ . >= 0, . < parameter.query.pageSize ],
            get(i): Object {
                id: Int [ . > 0 ],
                message: String
            }
        }
    }
}

endpoint "/post" POST {
    request: JsonRequest {
        body: Object {
            message: String,
            id: Int
        }
    },
    response: JsonResponse {
        body: Object {
            message: String [ . = 'hello: ' + request.body.message ]
        }
    }

}