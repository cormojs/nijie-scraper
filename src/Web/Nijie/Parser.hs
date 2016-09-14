{-# LANGUAGE OverloadedStrings #-}
module Web.Nijie.Parser where

-- internal
import Web.Nijie.Types

-- html-conduit
import Text.XML.Cursor (($//), (&//), (>=>))
import qualified Text.HTML.DOM   as HTML
import qualified Text.XML        as XML
import qualified Text.XML.Cursor as XMLC
import qualified Text.Blaze.Html as BlazeHtml (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- exception
import qualified Control.Exception as Exception

-- regex-posix
import Text.Regex.Posix ((=~))

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.UTF8  as BSUTF8
import qualified Data.ByteString.Lazy  as BSLazy
import qualified Data.ByteString.Char8 as Char8
import qualified Codec.Binary.UTF8.String   as UTF8Codec

-- text
import Data.Text  (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as TextIO
import qualified Data.Text.Encoding    as TextEnc
import qualified Data.Text.Lazy        as TextL
import qualified Data.Text.Lazy.IO     as TextLIO

-- list
import qualified Data.List as List

-- Control
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans as Trans
import Control.Applicative ((<$>))

import Debug.Trace (trace)

description :: XML.Document -> String
description doc =
  let [s] = XMLC.fromDocument doc
            $// XMLC.attributeIs "id" "view-honbun"
            &// XMLC.attributeIs "class" "m-bottom15"
      descs = XMLC.child s >>= XMLC.content
  in Text.unpack $ Text.concat descs

bookmarkId :: XML.Document -> ByteString
bookmarkId doc =
  let [elem] = XMLC.fromDocument doc
              $// XMLC.attributeIs "name" "bookmark_id"
  in TextEnc.encodeUtf8 $ head $ XMLC.attribute "value" elem

tags :: XML.Document -> [ByteString]
tags doc =
  let s = XMLC.fromDocument doc
          $// XMLC.attributeIs "id" "view-tag"
          &// XMLC.attributeIs "class" "tag"
          &// XMLC.attributeIs "class" "tag_name"
  in map (TextEnc.encodeUtf8 . toTagName) s
  where toTagName s =
          let (e:_) = s $// XMLC.element "a"
          in head $ XMLC.child e >>= XMLC.content

links :: ListAPI -> XML.Document -> [Link]
links api doc = case go api of
  [] -> Exception.throw NotLoggedInException
  lst -> lst
  where go (Illust _) = map njeLikeCursorToNjeLink $ cursor
                        $// XMLC.attributeIs "id" "main-left-main"
                        &// XMLC.attributeIs "class" "nijie mozamoza illust_list"
        go (Like _) = map njeLikeCursorToNjeLink $ cursor
                      $// XMLC.attributeIs "id" "main-left-main"
                      &// XMLC.attributeIs "class" "nijie mozamoza illust_list"
        go (Fav _ _)  = map njeBookmarkCursorToNjeLink $ cursor
                        $// XMLC.attributeIs "class" "nijie-bookmark"
        go (FavFolder _ _ _ _)  = map njeBookmarkCursorToNjeLink $ cursor
                                  $// XMLC.attributeIs "class" "nijie-bookmark"
        go (Rank _) = map njeOkazuCursorToNjeLink $ cursor
                         $// XMLC.attributeIs "id" "okazu_list"
                         &// XMLC.element "a"
        go (UserIllust user) = map (njeUserIllustCursorToNjeLink user) $ cursor
                                  $// XMLC.attributeIs "class" "mem-index clearboth"
                                  &// XMLC.attributeIs "class" "nijie"
        go (UserBookmark user _) =
          map (njeUserIllustCursorToNjeLink (User "" "-1")) $ cursor
          $// XMLC.attributeIs "class" "mem-index clearboth"
          &// XMLC.attributeIs "class" "nijie"
        go (Search _ _ _) = map njeLikeCursorToNjeLink $ cursor
                               $// XMLC.attributeIs "id" "main-left-main"
                               &// XMLC.attributeIs "class" "nijie mozamoza illust_list"
        cursor = XMLC.fromDocument doc

bookmarkFolders :: XML.Document -> [ListAPI]
bookmarkFolders doc =
  map cursorToList $ (XMLC.fromDocument doc)
  $// XMLC.attributeIs "id" "main-right"
  &// XMLC.attributeIs "id" "pro"
  &// XMLC.attributeIs "class" "bookmark_sort_list  "
  where cursorToList cursor =
          let [link] = cursor $// XMLC.element "a"
              [url]  = XMLC.attribute "href" link
              [nameText] = XMLC.content $ head $ XMLC.child link
              [id]   = XMLC.attribute "tag_id" cursor
              name = TextEnc.encodeUtf8 $ Text.strip nameText
          in FavFolder name (TextEnc.encodeUtf8 id) FavAsc 1

size :: ListAPI -> XML.Document -> Int
size api doc = go api
  where
    cursor = XMLC.fromDocument doc
    pattern = "([0-9,]+)" :: ByteString
    read' str = read $ List.delete ',' str
    extract :: ByteString -> Int
    extract text = read' $ BSUTF8.toString $ text =~ pattern
    convert =
      TextEnc.encodeUtf8 . head . XMLC.content . head . XMLC.child
    go (Search _ _ _) =
      let [em] = cursor
            $// XMLC.attributeIs "id" "search_result"
            &// XMLC.element "em" in
      extract $ convert em
    go (Like _) =
      let [em] = cursor
            $// XMLC.attributeIs "id" "main-left-main"
            &// XMLC.element "em" in
      extract $ convert em
    go (Illust _) =
      let [em] = cursor
            $// XMLC.attributeIs "id" "main-left-main"
            &// XMLC.element "em" in
      extract $ convert em
    go (Fav _ _) =
      let [span] = cursor
            $// XMLC.attributeIs "id" "main-left-none"
            &// XMLC.element "span" in
      extract $ convert span
    go (UserBookmark _ _) =
      let [em] = cursor
            $// XMLC.attributeIs "class" "mem-indent float-left"
            &// XMLC.element "em" in
      extract $ convert em
    go api = length $ links api doc

nextPage :: ListAPI -> XML.Document -> Maybe ListAPI
nextPage api@(Illust page) doc
  | hasNextPage api doc = Just $ Illust $ page+1
  | otherwise = Nothing
nextPage api@(Like page) doc
  | hasNextPage api doc = Just $ Like $ page+1
  | otherwise = Nothing
nextPage api@(Fav s page) doc
  | hasNextPage api doc = Just $ Fav s $ page+1
  | otherwise = Nothing
nextPage api@(Search str sort page) doc
  | hasNextPage api doc = Just $ Search str sort $ page+1
  | otherwise = Nothing
nextPage api@(UserBookmark user page) doc
  | hasNextPage api doc = Just $ UserBookmark user $ page+1
  | otherwise = Nothing
nextPage _ _ = Nothing

hasNextPage :: ListAPI -> XML.Document -> Bool
hasNextPage api doc = go api
  where
    cursor = XMLC.fromDocument doc
    hasRelNext = not $ null $ cursor $// XMLC.attributeIs "rel" "next"
    go (Illust _)      = hasRelNext
    go (Like _)        = hasRelNext
    go (Fav _ _)         = hasRelNext
    go (Search _ _ _ ) = hasRelNext
    go (UserBookmark _ _) = hasRelNext
    go _ = False

toUserId :: Text -> ByteString
toUserId text   = TextEnc.encodeUtf8 text =~ pattern
  where pattern = "([0-9]+)" :: ByteString

toAuthorId :: Text -> ByteString
toAuthorId text = TextEnc.encodeUtf8 text =~ pattern
  where pattern = "([0-9]+)" :: ByteString

toThumbUrl = ("http:"++) . Text.unpack

toKind []     = Single
toKind [kImg] = case XMLC.attribute "alt" kImg of
  ["同人"] -> Doujin
  ["漫画"] -> Manga
  ["アニメ"] -> Anime
  _        -> Single

njeUserIllustCursorToNjeLink :: User -> XMLC.Cursor -> Link
njeUserIllustCursorToNjeLink author cursor =
  let [dao]   = cursor $// XMLC.attributeIs "class" "nijiedao"
      kImg    = cursor $// XMLC.attributeIs "class" "thumbnail-icon"
                       &// XMLC.element "img"
      kAnime  = cursor $// XMLC.attributeIs "class" "thumbnail-anime-icon"
      [link]  = dao $// XMLC.element "a"
      [thImg] = dao $// XMLC.element "img"
      [title] = XMLC.attribute "title" link
      [url]   = XMLC.attribute "href" link
      [thUrl] = XMLC.attribute "src" thImg
  in Link { illustId = toUserId url
          , thumbUrl = toThumbUrl thUrl
          , illustTitle = TextEnc.encodeUtf8 title
          , author   = author
          , kind     = toKind kImg }


njeLikeCursorToNjeLink :: XMLC.Cursor -> Link
njeLikeCursorToNjeLink cursor =
  let [dao]   = cursor $// XMLC.attributeIs "class" "nijiedao"
      kImg    = cursor $// XMLC.attributeIs "class" "thumbnail-icon"
                       &// XMLC.element "img"
      [auSpn] = cursor $// XMLC.element "span"
      [auIdA] = XMLC.parent auSpn
      [link]  = dao $// XMLC.element "a"
      [thImg] = dao $// XMLC.element "img"
      [title] = XMLC.attribute "title" link
      [url]   = XMLC.attribute "href" link
      [thUrl] = XMLC.attribute "src" thImg
      [auN]   = XMLC.content $ head $ XMLC.child auSpn
      [auId]  = XMLC.attribute "href" auIdA
  in Link { illustId = toUserId url
          , thumbUrl = toThumbUrl thUrl
          , illustTitle    = TextEnc.encodeUtf8 title
          , author   = User (TextEnc.encodeUtf8 auN) (toAuthorId auId)
          , kind     = toKind kImg
          }

njeBookmarkCursorToNjeLink :: XMLC.Cursor -> Link
njeBookmarkCursorToNjeLink cursor =
  let [dao]   = cursor $// XMLC.attributeIs "class" "picture"
                       &// XMLC.attributeIs "class" "nijiedao"
      kImg    = cursor $// XMLC.attributeIs "class" "bookmark-thumbnail-icon"
                       &// XMLC.element "img"
      [authP] = cursor $// XMLC.attributeIs "class" "kazu"
      [authA] = authP  $// XMLC.element "a"
      [tleP]  = cursor $// XMLC.attributeIs "class" "title"
      title = case XMLC.child tleP of
        [s] -> s
        (_:s:_) -> s
      [auId] = XMLC.attribute "href" authA
      auNames = concatMap XMLC.content $ XMLC.child authA
      [link]  = dao $// XMLC.element "a"
      [url]   = XMLC.attribute "href" link
      [thImg] = dao $// XMLC.element "img"
      [thUrl] = XMLC.attribute "src" thImg
  in case auNames of
    [auName] ->
      Link { illustId = toUserId url
           , thumbUrl = toThumbUrl thUrl
           , illustTitle    = TextEnc.encodeUtf8 $ head $ XMLC.content title
           , author   = User (TextEnc.encodeUtf8 auName) (toAuthorId auId)
           , kind     = toKind kImg
           }
    _ -> Exception.throw ParseExcepiton

njeOkazuCursorToNjeLink :: XMLC.Cursor -> Link
njeOkazuCursorToNjeLink link =
  let id = head $ XMLC.attribute "href" link
      [titleC] = link $// XMLC.attributeIs "class" "rank"
                      &// XMLC.attributeIs "class" "title"
      [thumbs] = link $// XMLC.attributeIs "class" "rank"
                      &// XMLC.attributeIs "class" "illust_block"
                      &// XMLC.attributeIs "class" "illust"
                      &// XMLC.element "img"
      [title]  = XMLC.content =<< XMLC.child =<< titleC $// XMLC.element "strong"
      [author] = XMLC.content =<< XMLC.child =<< titleC $// XMLC.element "span"
      kind = link
             $// XMLC.attributeIs "class" "okazu-thumbnail-icon"
             &// XMLC.element "img"
  in Link { illustId       = toUserId id
          , thumbUrl = toThumbUrl $ head $ XMLC.attribute "src" thumbs
          , illustTitle    = TextEnc.encodeUtf8 title
          , author   = User (TextEnc.encodeUtf8 $ Text.drop 4 author)
                                     "-1"
          , kind     = toKind kind
          }
