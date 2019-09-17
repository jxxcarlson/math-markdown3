module Request exposing (..)

import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Api.Mutation as Mutation
import RemoteData exposing (RemoteData)
import Document exposing(Document)
import Api.InputObject as InputObject
import Api.Scalar
import Time


type RequestMsg =
   = GotResponse Model

 type alias Model =
     RemoteData (Graphql.Http.Error Response) Response

endpoint = "https://graphql.fauna.com/graphql"


requiredFieldsOfDocument : Document -> InputObject.DocumentInputRequiredFields
requiredFieldsOfDocument document =
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

--
--createDocument : Document -> Cmd RequestMsg
--createDocument document =
--    Mutation.createDocument (InputObject.buildDocumentInput document)
--            |> Graphql.Http.mutationRequest endpoint
--            |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
--


