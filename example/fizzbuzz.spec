type Main = Array {
    length: 20,
    get(i) : 
        'fizzbuzz' ? (i+1) % 15 = 0
      | 'fizz' ? (i+1) % 3 = 0 && (i+1) % 5 > 0
      | 'buzz' ? (i+1) % 5 = 0 && (i+1) % 3 > 0
      | Int[ . = i + 1] ? (i+1) % 5 > 0 && (i+1) % 3 > 0
}