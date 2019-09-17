
# NOTES

The app is deployed on Netlify at [focused-hodgkin-3d0fb4.netlify.com](https://focused-hodgkin-3d0fb4.netlify.com/)


## Commands

- run (dev): `parcel public/index.html`
- run (development): `yarn run parcel public/index.html`
-  build: `yarn run parcel build public/index.html`
    
## Fauna DB

[Tutoial](https://docs.fauna.com/fauna/current/start/cloud)

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

```
mutation CreateDocument {
   createDocument(data: {
   id: 2
   author: 1
   title: "Measuring atoms"
   content: "But they are so small!"
   }) {
       title
       content
   }
}
```

### Queries

``
query FindAllDocuments {
  allDocuments {
    data {
      _id
      title
      content
    }
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