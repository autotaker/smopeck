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