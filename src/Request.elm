module Request exposing (..)

import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Api.Mutation as Mutation
import RemoteData exposing (RemoteData)
import Document exposing(Document)
import Api.InputObject as InputObject
import Api.Scalar
import Api.Object.Document
import Time


type RequestMsg =
    GotResponse RemoteDataResponse

type alias RemoteDataResponse =
     RemoteData (Graphql.Http.Error Document) Document

endpoint = "https://graphql.fauna.com/graphql"

documentRequiredArguments : Document -> Mutation.CreateDocumentRequiredArguments
documentRequiredArguments document =
    { data = documentInput document }

documentInput : Document -> InputObject.DocumentInput  --RequiredFields
documentInput document =
        {   identifier = document.identifier
          , title = document.title
          , author = Api.Scalar.Id document.authorID
          , content = document.content
          , tags = document.tags
          , timeCreated = Time.posixToMillis document.timeCreated // 1000
          , timeUpdated = Time.posixToMillis document.timeUpdated // 1000
          , public = document.public
          , children = document.children
        }

-- documentSelectionSet : SelectionSet b Document
documentSelectionSet =
    SelectionSet.succeed Document
        |> with Api.Object.Document.identifier
        |> with Api.Object.Document.title
        |> with Api.Object.Document.author
        |> with Api.Object.Document.content
        |> with Api.Object.Document.tags
        |> with Api.Object.Document.timeCreated
        |> with Api.Object.Document.timeUpdated
        |> with Api.Object.Document.public
        |> with Api.Object.Document.children


createDocument : Document -> Cmd RequestMsg
createDocument document =
    Mutation.createDocument (documentRequiredArguments document) documentSelectionSet
         |> Graphql.Http.mutationRequest endpoint
         |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
