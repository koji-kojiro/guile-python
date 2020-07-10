# guile-python
```scheme
(use-modules (python import))

(import http.server)
(import socketserver)

(let* ((handler #.http.server.SimpleHTTPRequestHandler)
       (httpd (#.socketserver.TCPServer #("" 8000) handler)))
  (#.httpd.serve_forever))
```

guile-python provides seamless interface to Python from GNU Guile. Note that the project is still in quite experimental stage.  

## Requirements
- GNU Guile 2.2+
- Python 3.3+

## Installation
```shell
$ ./bootsrap
$ ./configure
$ make
$ sudo make install
```

## Usage
guile-python consists of three modules, `(python eval)`, `(python import)` and `(python core)`:  

- `(python eval)` provides a few procedures to run python code given as string.
- `(python import)` provides `import` macro (note that this shadows built-in `import`) and `#.` syntax, which allows python-style member accessing.  
- `(python core)` is a core module used internally.

Python object is converted to Scheme object according to the following table:  

| Python          | Scheme                          |
|:---------------:|:-------------------------------:|
| Integer         | Integer                         |
| Float           | Real                            |
| Bytes           | Character set                   |
| String          | String                          |
| List            | List                            |
| Tuple           | Vector                          |
| Dictionary      | Hash table                      |
| True            | #t                              |
| False           | #f                              |
| None            | #nil                            |  
| Callable object | Procedure                       |
| Object          | Python object (wrapped pointer) | 

### Module:(python eval)
- **Procedure:python-eval code**  
&nbsp; &nbsp; Equivelent to `eval(code)` in Python. Returns evaluation result.  

- **Procedure:python-exec code**  
&nbsp; &nbsp; Equivelent to `exec(code)` in Python. Returns `#t`.  


### Module:(python import)
- **Macro:import module**  
&nbsp; &nbsp; Equivelent to `import module` in Python. Note that this shadows built-in `import`.

- **Reader Extension:#.object.attribute...**  
&nbsp; &nbsp; Equivelent to `foo.bar...` in Python. The value is automatically converted between Python and Scheme.  

## Author
[TANI Kojiro](https://github.com/koji-kojiro) (kojiro0531@gmail.com)

## License
guile-python is distributed under [GPLv3](LICENSE).
