# DirHound #

DirHound is a web crawler. It also performs bruteforcing in order to
find files and directories in the target website.

## Usage ##
-----

### URL ###

DirHound requires a base URL which will be the start point of the 
crawling session:

```Shell
./DirHound http://www.example.com
```

### Wordlist ###

Bruteforcing requires a wordlist containing relative paths that will be
appended to every directory found on the crawled website. 

The wordlist should be provided in a file containing one path per line,
such as:

```Shell
admin/
admin.php
phpmyadmin/
login.php
```

A default wordlist is provided, containing some commonly used directory
and file names. If you want to provide your own wordlist, you can use
the *-w* parameter:

```Shell
./DirHound -w /some/path/wordlist http://www.example.com
```

### Output ###

DirHound generates an output file containing the links discovered while 
crawling and all of the successfully bruteforced paths. By default, the
output will be stored in a file named *dirhound.out* but you can provide
your own path: 

```Shell
./DirHound -o /tmp/my_crawl_status http://www.example.com
```
