# R.template

This is a project template for an R template

Once you have cloned this project you need to do a global search for "R.template" and replace it with your application name.

Then update both DESCRIPTION and project-package.Rd with title, description and author information. Put your library dependencies in DESCRIPTION and import then with import("foo") in NAMESPACE.

### Development

In R, within the (renamed) R.templates directory:
    
    devtools::load_all()
    
Loads the package currently under development.

### Documenting

With the package loaded, you can create a function's documentation template with:
    
    prompt(<funcname>, "man/<funcname>.Rd")

### Testing
In R, within the (renamed) R.templates directory:

    devtools::test()

### Building

Run from bash in the root directory:
    R CMD build <package name>

### Checking package

After, building run:

    R CMD check <package tarball>
    
It will give you a list of things to fix. Ignore the warning related to licencing.
    
### Installing locally

Run:
    
    R CMD install <package name>
