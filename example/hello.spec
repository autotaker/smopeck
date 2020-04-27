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

endpoint "/profile" GET {
    response: JsonResponse {
        body: Object {
            id : Int [ . > 0 ],
            name : String [ . =~ r'[a-zA-Z0-9]{1,10}' ]
        }
    }
}

endpoint "/regex" GET {
    response: JsonResponse {
        body: Array {
            length: Int [ . = 10 ],
            get(i): String [ . =~ r'[a-z]{4}-[0-9]{5}' ]
        }
    }
}

endpoint "/regex/check" POST {
    request: JsonRequest {
        body: Object {
            phone: String [ . =~ r'0[0-9]+-[0-9]+-[0-9]+']
        }
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
            key : String 
        }
    },
    response: JsonResponse {
        body: Array {
            length: Int [ . >= 0, . < 10 ],
            get(i): Object {
                id: Int [ . > 0 ],
                content: String [ . =~ r'.*' + parameter.query.key + r'.*' ]
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

endpoint "/user/:id/profile/email" POST {
    parameter : Parameter {
        path: Object {
            id : Int [ . > 0]
        }
    },
    request: JsonRequest {
        body: Object {
            email: String [ . =~ r'[a-zA-Z0-9.!#$%&\'*+\/=?^_`{|}~-]+@[a-zA-Z0-9-]+(\.[a-zA-Z0-9-]+)*' ]
        }
    },
    response: JsonResponse
}

type Follower = Object {
    userId : String,
    age: Int [ . > 0, . < 100 ]
}
type Pos = Int [ . > 0]

endpoint "/user/:userId/followers/list" GET {
    parameter: Parameter {
        path: Object {
            userId : String
        },
        query: Object {
            pageNum : Pos,
            pageSize : Pos [ . <= 100 ]
        }
    },
    response: JsonResponse {
        body: Object {
            userId : String [ . = parameter.path.userId ],
            followers : Array {
                get(i) : Follower,
                length: Pos [ . <= parameter.query.pageSize ]
            }
        }
    }
}