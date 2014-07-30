ztask
=====
###Task Description
Write a FastCGI or HTTP server in Haskell that provides a restful API for managing an inventory of shoes:

* POST new shoes as a JSON body, with attributes "description", "color", "size", and "photo". The "photo" attribute should be a base-64 encoded string representing a JPEG image (think "data URI"). For example:
   
```json
{"description": "SADIE Faux Suede Heels with Bow", "color": "red", "size": "35", "photo": "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgMCAgMDAwMEAwMEBQgFBQQEBQoHBwYIDAoMDAsKCwsNDhIQDQ4RDgsLEBYQERMUFRUVDA8XGBYUGBIUFRT/2wBDAQMEBAUEBQkFBQkUDQsNFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBT/wAARCAA2ACUDASIAAhEBAxEB/8QAGwABAQACAwEAAAAAAAAAAAAAAAkGBwEFCAr/xAAxEAABAwMDAgUCBAcAAAAAAAABAgMEBQYRAAchCBIJEyIxUTJhF0GCkSMzQlJxgaH/xAAaAQABBQEAAAAAAAAAAAAAAAAAAgMEBQcG/8QAKxEAAQMDAgMHBQAAAAAAAAAAAQACAwQFERIhIjFxBhMyQVFhgaHB0eHx/9oADAMBAAIRAxEAPwCqClJQMqISPknXPGpgeKRvTNuC7U7a0+oPRaVTGG35jbKyA9KWAtPdj3CElGPglWs18N/ruO4UWHtTuJPIuyKnyaRVJCuai2kfyXFH3eSBwT9YGD6h6mhICcLpKqxVFLRw1bjnvBqx6D+YPQ+xVCuPjTj40xpjTq5tONNMaaEKSviRbJ1qxt3pd5+WqTb1xL8xmTyfKfCfWyo/keO5Pyk8fSceHno1RolYj1ykyHYEyM+mTGlMEpU08ghQUlQ9iDj/AJq3XiFW8mv9LtwKUgKXCmQZCCRkpJkttEj9LqtS3bsuVZN6XFt7c7PlS4z6h5CvoWsDlSPstHaoEe4xqvk4HEBbXaqxl5tbYp/HHt8AAA/UZ/aq10T9TUfqe2Zh1qQW2rpppEGuREcdr4HDoH5IcHqH5A9w/p1v/wDfUH+nPqCqnRrvy7Ukoen27KIi1enoIzJjE5S4gHjzEZ7h+pOQFHVtLD3YtHcyzIV123X4VSoMsJDctDwSErUQA2sHBQvJAKFYIJxjUuN+oLL7pQOo5yGjhPL8LLP300wTpp1Ui6K+bHo249qVG27hhmfR56AiRHDq2ioBQUMLQQpJCkgggg8anB4ou1kml3dQL8hxVR1ONCDKlMjAUpHLKyf7u3KT9ko1T3H21je4W3dB3StSbblyQG6jSpiO1xpfuPggjkEHkEcjSHsDhhXNpuLrZVsn5tGxHqDz+x6gL5/Kg63e1WpzNecDDaXAl2ayj+IGyecge/8Aof4B9tbAvna/8MaYmo27PcqNjTXEOImJX6mncHsRISDjux3dqx6VDPbg9yR7wvjwmLSqSlu2zdtWpKlZwxMSiQ2PsOEq/cnWvZvhLXfPhsQHNzWDT2HFOtsrgLUkKPurt80DP31BMMgIwdlpMfaW2RytqYTpd5gt8uozg/K9PeH1uBeO4uxDU+7JaaoyxKMal1IvIcekR0oTkOkKKu9CypHrCVYSD6shRa7/AKP+lxXSxZVWoi7nkXM7UpglrUqMIzTJCAnCG+9fJxyrPOE8DHLU9uQBlZhc5Yp6yWWHGlxyMDA39lvzA0wNNNKVYnGuONNNCEOmmmhC/9k="}
```

* GET a shoe as an HTML page listing the shoe details, where the photo is served as an <img> tag with "src" pointing to a path on the local filesystem (i.e. the photo must be accessible as a local file, not as a data URI).
   
* GET a list of shoes as an HTML page with hyperlinks to all available shoes.

###Installing and running
Clone, then build using a sandbox.
```
$ git clone https://github.com/tuleism/ztask
$ cd ztask
$ cabal sandbox init
$ cabal install --dependencies-only
$ cabal build
```

Running arguments:
```
$ ztask -h
Ztask

  -h         --help, --usage, --version  Display help and version information.
             --undefok                   Whether to fail on unrecognized command line options.
  -c STRING  --config=STRING             Config file's location (default: , from module: Config)
```

This is the default config. It will be used if there user doesn't provide a config file.
```
# port the app will listen on. Default to 3000.
port=3000
# sqlite database connection string. Default to example.db file.
dbConnectionString="example.db"
# photo directory to save uploaded photo. Default to current directory
photoDir="."
```

###Usage
With default config, after running, we can:
* View all shoes:
```
http://localhost:3000/shoes
```
* View shoes with id 1
```
http://localhost:3000/shoes/1
```
* Post shoes using json in file example.json
```
$ curl -X POST -d @example.json http://localhost:3000/shoes -H "Content-Type: application/json"
```

###Implementation
Some core libraries that powered Ztask:
* Scotty: web framework.
* Persistent: abstraction over backend database. Here, Ztask use Sqlite.
* Aeson: dealing with JSON.
