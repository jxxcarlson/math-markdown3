
# NOTES

The app is deployed on Netlify at [focused-hodgkin-3d0fb4.netlify.com](https://focused-hodgkin-3d0fb4.netlify.com/)

## Netlify 

[How to use Netlify Functions in Elm](https://www.freecodecamp.org/news/how-to-use-netlify-functions-in-elm/?utm_campaign=Elm%20Weekly&utm_medium=email&utm_source=Revue%20newsletter)


elm-graphql https://math-markdown.netlify.com/graphql --header fnADYYvKu5ACB1F7JQBRg9LPpUew3jjVd6VqHu11

## Commands

- run (dev): `parcel public/index.html`
- run (development): `yarn run parcel public/index.html`
-  build: `yarn run parcel build public/index.html`
    
## Fauna DB

[Tutorial](https://docs.fauna.com/fauna/current/start/cloud)

[Dashboard](https://dashboard.fauna.com/)

[Fauna shell](https://fauna.com/blog/introducing-fauna-shell)

Database name: 7ccf0e66-539e-4cdd-80ae-18b24ae9ae06

To get started, import your GraphQL Schema using a .gql or .graphql file.

### Schema

```
type Document {
   id:ID!
   title:String!
   content:String
   author:ID!
   tags:[String]
}

type Query {
  # Get one document
  document(id: ID!): Document
  # Get all documents
  allDocuments: [Document!]!
}
```


### Mutations

```bash
mutation CreateDocument {
    createDocument(data: {
      identifier: "t2"
      title: "Measuring atoms"
      author: 1
      content: "But they are so small!"
      tags: []
      timeCreated: 1568700834
      timeUpdated: 1568700834
      public: true
      children: []
      
     }) {
         title
         content
  }}
    
```

```bash
Create(Collection("Document"), { data: {
    id: 33,
    identifier: "t1",
    title: "Measuring atoms",
    author: 1,
    content: "But they are so small!",
    tags: [],
    timeCreated: 1568700834,
    timeUpdated: 1568700834,
    public: true,
    children: []
   } 
 }
)
```

### Queries


#### Find all documents

```
query FindAllDocuments {
  allDocuments {
    data {
      _id
      identifier
      title
      content
    }
  }
}
```

#### Find document by author identifier

```
query FindDocumentByAuthor {
  documentsByAuthor(author: "1") { 
      _id
      identifier
      title
      content
    }
  }
```
## Links

[Math-markdown3 on Netlify](https://focused-hodgkin-3d0fb4.netlify.com/)

[FaunaDB](https://www.netlify.com/blog/2019/09/10/announcing-the-faunadb-add-on-for-netlify/)

[Domenkozar: Parcel & Elm](https://discourse.elm-lang.org/t/using-elm-and-parcel-for-zero-configuration-web-asset-management/2576)

[yarn](https://yarnpkg.com/en/docs/cli/run)

[Using yarn (start)](https://blog.hercules-ci.com/elm/2018/11/21/using-elm-and-parcel-for-zero-configuration-web-asset-management/)


[Setting up Elm with Parcel](https://kawamurakazushi.com/20190118-setting-up-elm-with-parcel/)

### Fauna tutorial

```
$ fauna create-database my_db
creating database my_db

  created database 'my_db'

  To start a shell with your new database, run:

  fauna shell 'my_db'

  Or, to create an application key for your database, run:

  fauna create-key 'my_db'
 ```$xslt

```