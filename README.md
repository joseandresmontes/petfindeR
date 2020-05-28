# petfindeR

The Petfinder API (Application Programming Interface) allows you to access the Petfinder database of hundreds of thousands of pets ready for adoption and over ten thousand animal welfare organizations. 

With the PetfindeR, you can:

* Search for and display pet listings based on pet characteristics, location, and status.
* Search for and display animal welfare organizations based on organization name, ID, and location.

An many other field! Look at the documentation to explore supported parameters.
___

## Installation and Setup
Install the latest version from github as follows.

```
devtools::install_github('JoseAndresMontes/petfindeR')
```

Each subscriber to the Petfinder API is uniquely identified by an Key and Key Secret, and every request to the API requires a token that can only be accessed by providing both of these credentials. After loading the `petfindeR` package, you should set your Key and Key Secret in your environment as shown below. This facilitates API calls without needing to specify your credentials after first setup.

```
library(petfindeR)
R.home(component = "home")
usethis::edit_r_environ()
```

Then set your Key and Secret Key exactly like this
```
PETFINDER_APP_KEY = x #Key Sequence
PETFINDER_APP_SECRET = y #Key Secret Sequence
```
Click [here](https://www.petfinder.com/developers/) to register with Petfinder and receive your credentials.
 
## Basic Usage
petfindeR provides easy to use function for the API service, with API parameters as function arguments. GetAnimals function gathers all available data. Available parameters are documented in the package.

For example:

```
data <- GetAnimals()
```

Be sure to check out the [API documentation](https://www.petfinder.com/developers/v2/docs/) to learn more about the API. 
