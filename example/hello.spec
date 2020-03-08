endpoint "/" GET {
    response: JsonResponse {
        body: String [ .= 'hello world!' ]
    }
}