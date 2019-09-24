# Hasura Notes

## GraphiQL

### Queries II

#### Get list of a particular user's documents 

```
{
  document (where: {authorIdentifier: {_eq: "jxxcarlson"}}){
    id
    title
  }
}
```

#### Get list of documents with info about their authors
```
query {
  document {
    id
    title
    user {
      id
      username
      email
    }
  }
}
```

#### List users and their documents

```
query {
  user {
    id
    username
    documents {
      id
      title
    }
  }
}
```

### Example

```

 {-
  query {
    user {
      id
      name
    }
  }
 -}

type alias User =
  {
    id : Int
    name: String
  }

type alias Users =
  List User

fetchUsersQuery : SelectionSet Users RootQuery
fetchUsersQuery =
    Query.user identity userListSelection

userListSelection : SelectionSet User Hasura.Object.User
userListSelection =
    SelectionSet.map2 User
        user.id
        user.name

-- Use the client provided by elm-graphql to make the GraphQL query request

makeGraphQLQuery : SelectionSet Users RootQuery 
   -> (Result (Graphql.Http.Error Users) Users -> msg) 
   -> Cmd msg
makeGraphQLQuery query msgType =
    query
        |> Graphql.Http.queryRequest "https://myapi.com/graphql"
        |> Graphql.Http.send msgType

```

### Queries

#### Get users 

```
{
  user {
    email
    id
    username
  }
}
```