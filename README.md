# R.template

This is a project template for an R package.

Once you have cloned this project you need to do a global search for "R.template" and replace it with your application name. Don't forget to change the folder name "R.template" to something better as well.

Then update both DESCRIPTION and project-package.Rd with title, description and author information. Put your library dependencies in DESCRIPTION and import then with import("foo") in NAMESPACE.

### Development

In R, within the (renamed) R.templates directory, with the devtools package loaded:
    
    load_all()
    
Loads the package currently under development. This setup to automatically happen each time you start R within that directory (one down from the project root).

### Documenting

The best way to produce documentation is with roxygen. Just add annotations above each function you want to document like the following:

```
 #' @title my_function
 #' @description Describe it here
 #' @param x Describe x
 #' @param y Describe y
 #' @returns what it returns
 my_function <- function(x,y) { ...
```

Then to produce the .Rd documentation files, call `document()`

### Testing
In R, within the (renamed) R.templates directory:

    test()

### Building

Run from bash in the root directory:
    R CMD build <package name>
        
This produces a tarball.

### Checking package

After, building run:

    R CMD check <package tarball>
    
It will give you a list of things to fix. Ignore the warning related to licencing.
    
### Installing locally

Run:
    
    R CMD install <package name>
