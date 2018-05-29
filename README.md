

## Features

- Typesafe SQL with [Beam](https://hackage.haskell.org/package/beam-core)
- Password encryption with scrypt
- Protected and Unprotected Endpoints with [Servant](https://haskell-servant.readthedocs.io/en/stable/)
- Pagination
- [Automated CRUD utilities](#crud)
- Automatic API generation with Servant Swagger and Swagger UI


#### This is also the Haskell Back-End for [Create-Haskstar-App](https://github.com/smaccoun/create-haskstar-app)


## Table Of Contents

Below is a list of useful features for building out CRUD apps

- [Conventions](#conventions)
  - [Pagination](#pagination)
- [Creating a resource](#resource)
- [Auto CRUD Generation](#crud)


### Conventions

#### Pagination

Queries expected to return more than one result should generally be paginated.
You can see an example of returning a pagianted result with `Database.Crud.getEntities`
A `PaginatedResult` looks like:

```
{
  "pagination": {
    "nextPage": null,
    "currentPage": 0,
    "count": 1,
    "perPage": 10,
    "previousPage": null,
    "totalPages": 1
  },
  "data": [
    {
      "baseEntity": {
        "content": "### Here's some content",
        "title": "My First Post"
      },
      "meta": {
        "createdAt": "2018-05-29T21:08:19.463033Z",
        "id": "4bfdc901-302f-4276-89e0-e0c27c87cb1c",
        "updatedAt": "2018-05-29T21:08:34.150376Z"
      }
    }
  ]
}
```

### Resource


Writeup coming soon. For now see 

`Api.Resource`
`Database.CRUD`


### CRUD


Writeup coming soon. For now see 

`Database.CRUD`

  
