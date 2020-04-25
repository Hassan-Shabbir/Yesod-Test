{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE QuasiQuotes                   #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE ViewPatterns                  #-}
{-# LANGUAGE DeriveGeneric                 #-} -- used with aeson
{-# LANGUAGE DeriveAnyClass                #-} -- used with aeson
{-# LANGUAGE ExtendedDefaultRules          #-} -- used with aeson and string literals
{-# LANGUAGE MultiParamTypeClasses         #-} -- required to use forms
{-# LANGUAGE GADTs                         #-} -- required to use persistent database
{-# LANGUAGE DerivingStrategies            #-} -- required to use persistent database
{-# LANGUAGE StandaloneDeriving            #-} -- required to use persistent database
{-# LANGUAGE UndecidableInstances          #-} -- required to use persistent database
{-# LANGUAGE GeneralizedNewtypeDeriving    #-} -- required to use persistent database

--------------------------------------------------------------------------------
-- Imports
import GHC.Generics -- read json object from session
import Data.Aeson -- read json object from session
import Control.Monad.Trans.Resource (runResourceT) -- connect db to yesod
import Control.Monad.Logger (runStderrLoggingT) -- connect db to yesod
import Database.Persist -- used with db
import Database.Persist.Sqlite -- used with db
import Database.Persist.TH -- used with db
import Control.Applicative ((<$>), (<*>)) -- used with forms
import Data.Time -- used in footer for year
import Data.Text (Text) 
import Data.Text.Encoding (decodeUtf8) -- auth read map
import Network.HTTP.Client.Conduit (Manager, newManager) -- auth
import qualified Data.Map as M -- auth display values in session map
import qualified Data.Text as T
import qualified Text.Read as TR (readMaybe)
import Yesod
import Yesod.Auth -- auth
import Yesod.Auth.Email -- auth
import Yesod.Auth.OAuth2.Google -- auth
--import Text.Latex -- latex support

--------------------------------------------------------------------------------
-- Datatype on which Yesod is run
data HelloWorld = HelloWorld ConnectionPool

--------------------------------------------------------------------------------
-- Define the database of the application
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
  name String
  age Int Maybe
  deriving Show

BlogPost
  title String
  authorId AuthorId
  deriving Show
|]

--------------------------------------------------------------------------------
-- Run the datatype on port 3000 on localhost
main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test3.sqlite3" connCount $
  \pool -> liftIO $ do
-- TODO remove comments below to enable db
--    runResourceT $ flip runSqlPool pool $ do
--      runMigration migrateAll
--      deleteWhere ([] :: [Filter Author]) -- truncates table
--      insert $ Author "Hassan" (Just 23)
--      insert $ Author "Maria" (Just 25)
--      insert $ Author "Abdul" (Just 29)
--      insert $ Author "Mohammad" (Just 60)
--      insert $ Author "Shamim" (Just 50)
    warp 3000 $ HelloWorld pool
  where connCount = 10

--------------------------------------------------------------------------------
-- Define all routes of the application
mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
/person PersonR POST
/teacher TeacherR GET
/math MathR GET
/json JsonR GET
/blog/#BlogId BlogIdR GET POST DELETE
|] 

--------------------------------------------------------------------------------
-- Info used to authenticate using Google Auth
clientId :: Text
clientId = "651837639044-vj9quhrfhcn1742hujini9i9uib3ndah.apps.googleusercontent.com"

clientSecret :: Text
clientSecret = "w7wgbXVSLQ9fmMKNBk9Sc6Jo"

--------------------------------------------------------------------------------
-- Data type instances
instance Yesod HelloWorld where
  makeSessionBackend _ = -- require login 3 hours after last visit
    fmap Just $ defaultClientSessionBackend (60 * 3) "config/client_session_key.aes"
  approot = ApprootStatic "http://localhost:3000" -- defines the root url
  authRoute _ = Just $ AuthR LoginR

  -- urlR, trying to write :: Bool
  isAuthorized TeacherR _ = isAdmin
  isAuthorized _ _ = return Authorized

isAdmin = do
  mUser <- maybeAuthId
  liftIO $ print mUser
  return $ case mUser of
    Nothing -> AuthenticationRequired
    -- my personal uid from google
    Just "google-uid:116710817805451389133" -> Authorized
    Just _ -> Unauthorized "You must be an admin to access this page."

-- defines the translation function for forms
instance RenderMessage HelloWorld FormMessage where
  renderMessage _ _ = defaultFormMessage

-- defines a way to access db by getting connection info
instance YesodPersist HelloWorld where
  type YesodPersistBackend HelloWorld = SqlBackend
  runDB action = do
    HelloWorld pool <- getYesod
    runSqlPool action pool

-- authentication
instance YesodAuth HelloWorld where
  type AuthId HelloWorld = Text
  --authenticate = return . Authenticated . credsIdent -- default
  -- TODO find out what things are important to keep ie. email, name, etc.
  authenticate c = do
    mapM_ (uncurry setSession) $ 
      [ ("credsIdent", credsIdent c)
      , ("credsPlugin", credsPlugin c)
      ] ++ credsExtra c
    sess <- getSession
    -- testing
    --liftIO $ print $ sess
    --liftIO $ putStrLn "!@!@!@Access Token:"
    --liftIO $ print $ decodeUtf8 <$> M.lookup "accessToken" sess
    --liftIO $ putStrLn "!@!@!@Credentials Identity:"
    --liftIO $ print $ decodeUtf8 <$> M.lookup "credsIdent" sess
    liftIO $ putStrLn "\n\n\n\n\n\n"
    liftIO $ do
      jsonInfo <- decodeUtf8 <$> M.lookup "userResponse" sess
    liftIO $ putStrLn "\n\n\n\n\n\n"
    --liftIO $ putStrLn "!@!@!@User Response:"
    --liftIO $ print $ decodeUtf8 <$> M.lookup "userResponse" sess
    return $ Authenticated "1"


--ya29.a0Ae4lvC0asJD9LdRNNnZBtuSkFBRoi2oK3q0lyWJKfFg8qTW1RiAQzXQqDbk6KcrW4_pO3lizWmiM64_FmY8peFSIsD_rnYybhXsmmRgbtcriPmqQRqeuBHbcVYwaZrkdnDcj5HPyTEb7zMuv9HD5wxP53MKHXY3rW2qf

{-fromList 
    [ ("accessToken","ya29.a0Ae4lvC0asJD.....")
    , ("credsIdent","google-uid:116710817805451389133")
    , ("credsPlugin","google")
    , ("person","Person 
        { pName = \"lasdfjls\"
        , pAge = Nothing
        , pUsername = \"hshabbi4\"
        , pPassword = \"sldkjfasdlfaslkfd\"
        }")
    , ("userResponse","
        { \"sub\": \"116710817805451389133\"
        , \"name\": \"Hassan Shabbir\"
        ,  \"given_name\": \"Hassan\"
        ,  \"family_name\": \"Shabbir\"
        ,  \"picture\": \"https://lh4.googleusercontent.com/-5Bd097iqwU0/AAAAAAAAAAI/AAAAAAAABbY/AAKWJJOxY8cmbML945iTr_nQbTJZJ-86kw/photo.jpg\"
        ,  \"email\": \"hassan149367@gmail.com\"
        ,  \"email_verified\": true
        ,  \"locale\": \"en\"}"
    ) ]
-}

  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins _ = [oauth2GoogleScoped ["email", "profile"] clientId clientSecret]
  maybeAuthId = lookupSession "_ID"

--------------------------------------------------------------------------------
-- Define the route handling functions

data Person = Person 
  { pName :: Text
  , pAge :: Maybe Int
  , pUsername :: Text
  , pPassword :: Text
  }
  deriving (Eq, Ord, Show, Read)

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
  <$> areq textField "Name:" Nothing -- Nothing used for default values
  <*> aopt (selectFieldList ages) "Age (Optional):" Nothing
  <*> areq textField "Username:" Nothing
  <*> areq longPasswordField "Password:" Nothing
  where
    ages :: [(Text, Int)]
    ages = zip (T.pack <$> show <$> as) as
    as = [0,5..30]

    longPasswordField = checkBool 
      (\t -> T.length t >= 7) 
      (T.pack "Password too short.") 
      passwordField

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost personForm
  mPerson <- lookupSession "person"
  authors <- runDB $ selectList [] [Asc AuthorAge]
  mAuthId <- maybeAuthId
  defaultPage $ do
    toWidgetHead
      [cassius|
        .hidden
          color: #CCCCCC
      |]
    [whamlet|
      $maybe sPerson <- mPerson
        $maybe person <- TR.readMaybe $ T.unpack sPerson
          <h2>Welcome Back #{pName person}!
        $nothing
          <h2>Learn Math!
      $nothing
        <h2>Learn Math!
      <p>This is a website that #
          <i>may
          \ be used by #{name} to teach students.
      <ul>Here are all of the users of this system:
        $forall Entity authorId author <- authors
          <li>#{authorName author}
      <p>The password length must be at least 7 characters.
    |]
    [whamlet|
      <form method=post action=@{PersonR} enctype=#{enctype}>
        ^{widget}
        <button>Submit
      <p>Google Authentication
      <p>Your current auth ID: #{show mAuthId}
      $maybe _ <- mAuthId
        <p>
          <a href=@{AuthR LogoutR}>Logout
      $nothing
        <p>
          <a href=@{AuthR LoginR}>Login
    |]
  where 
    name = "Ahson"
    showHidden = True

getMathR :: Handler Html
getMathR = defaultPage $ do
  toWidget 
    [cassius|
      .test > *
        padding-bottom: 200px
      body
        font-size: 130%
    |]
  [whamlet|
    <h2>Mingdao International Department
    <h3>An IB world School
    <h4>Name: Hassan Shabbir
    <h4>Teacher: Mr. Abdul Qadir Shabbir
    <h5>Mingdao High School International Secondary Department
    <h5>Subject & Grade: Grade 11 HL Mathematics
    <h5>Assessment Type: Summative Test
    <h5>Unit 4: Sequences & Series, Binomial Theorem, and Counting Principles
    <h5>Time: 90 minutes
    <h5>INSTRUCTIONS:
    <ol>
      <li>Read the questions and directions carefully.
      <li>Write your final answer in the box provided, if provided.
      <li>A translation dictionary may not be used.
      <li>
        <b>GOOD LUCK!
    <p>
      <u>
        <i>Instructions:#
      Write down your final answer inside the box if provided. Show all your working
      for each question to receive partial method marks.
    <ol .test>
      <li>Expand \((3-x)^4\) in descending powers of \(x\) and simplify your final answer. [4 marks]
      <li>Expand and simplify \((\sqrt{3} + 2)^5\) giving your answer in the form \(a + b\sqrt{3}\) where \(a, b \in \Z\). [5 marks]
      <li>Consider the following series: \(-13 -5 +3 + \ldots + 83\).
        <ol type=a>
          <li>Express the series in sigma notation. [4 marks]
          <li>Find the sum of the series. [2 marks]
      <li>Find the value of \(k\) if \(\sum _ {r = 1} ^ {\infty}k(\frac{1}{3})^r = 7\). [4 marks]
      <li>Consider the infinite geometric series \(1-(1-2x)+(1-2x)^2-(1-2x)^3+\ldots\enspace\):
        <ol type=a>
          <li>Find the range of values of \(x\) so that the series converges to a finite sum.
          <li>Assuming that \(x\) lies within this range found above, find the sum to infinity in terms of \(x\).
      <li>An arithmetic sequence: \(u_1, u_2, u_3, \ldots\) has \(u_1 = 1\) and a common difference \(d = 0\). Given that \(u_2, u_3\) and \(u_6\) are also the first three terms of a geometric sequence:
        <ol type=a>
          <li>Find the value of \(d\). [4 marks]
          <li>Given that \(u_N = -15\), determine the value of \(\sum_ {r=1} ^ {N} u_r\). [2 marks]
      <li>Find the exact value of the constant term in the expansion of \((4x^2 - \frac{3}{2x})^ {12}\). [5 marks]
      <li>The sum of the first \(16\) terms of an arithmetic sequence is \(212\) and the fifth term is \(8\).
        <ol type=a>
          <li>Find the first term and the common difference. [4 marks]
          <li>Find the smallest value of \(n\) such that the sum of the first \(n\) terms is greater than \(600\). [2 marks]
      <li>Find the term independent of \(x\) (i.e. the constant term) in the expansion: \(\left(1-2x^2\right)^3\left(\frac{3}{x}+2x^2\right)^6\). [6 marks]
      <li>Consider the geometric sequence: \(u_1, u_2, u_3, \ldots\) which has a common ratio \(r\). Consider a second sequence \(a_1, a_2, a_3, \ldots\) whose \(n^ {th}\) term is given by \(a_n = \log |u_n|\).
        <ol type=a>
          <li>Show that \(a_n\) is arithmetic, stating its common difference \(d\) in terms of \(r\). [4 marks]
          <li>A particular geometric sequence has \(u_1 = 3\) and a sum to infinity of 4. Find the value of \(d\). [2 marks]
      <li>Twelve students are to take an exam in advanced combinatorics. The exam room is set out in three rows of four desks, with the invigilator at the front of the room, as shown in the following diagram: \[ \begin{matrix} & Invigilator & \\ Desk_1 & Desk_2 & Desk_3 & Desk_4 \\ Desk_5 & Desk_6 & Desk_7 & Desk_8 \\ Desk_9 & Desk_ {10} & Desk_ {11} & Desk_ {12} \\ \end{matrix} \] Two of the students, Helen and Nicky, are suspected of cheating in a previous exam.
        <ol type=a>
          <li>Find the number of ways the twelve students can be arranged in the exam hall. [1 mark]
          <li>Find the number of ways the students may be arranged if Helen and Nicky must sit so that one is directly behind the other (with no desk in between). For example, \(Desk_5\) and \(Desk_9\). [2 marks]
      <li>Consider the expansion of \(\left( 1 + x \right)^n\) in increasing powers of \(x\), where \(n \geq 3\). The coefficient of the second, third and fourth terms of the expansion are consecutive terms of an arithmetic sequence.
        <ol type=a>
          <li>Write down the first four terms of the expansion. [2 marks]
          <li>Show that \(n^3-9n^2+14n=0\). [4 marks]
          <li>Hence, find the value of \(n\). [2 marks]
      <form>
        <li style=padding-bottom:0>Test question: what is the value of the following expression? \(\sum_ {i=0} ^ {5}\). Note: you can use latex command name (without backslash) and two spaces for autocomplete. Equals signs are automatically aligned.
        <textarea #input oninput="f()" rows=4 cols=80 style=padding-bottom:0 spellcheck=false>
        <div #output>
  |]
  toWidget
    [julius|
      // TODO syntax highlighting
      var input = document.getElementById('input');
      var output = document.getElementById('output');

      function f() {
        // lame autocompletion, works quite well surprisingly, is it too slow?
        // TODO ask ahson which commands he will use with his students
        var sPrime = input.value
          // paired commands
          .replace(/\(  /, '\\left( \\right)')
          .replace(/matrix  /, '\\begin{matrix} \\end{matrix}')
          // bounded
          .replace(/(sum|prod)  /, '\\$1_\{}^\{} ')
          // arity 0
          .replace(/(R|N|Z|therefore|mod|pm|Alpha|Beta|Gamma|Delta|Epsilon|Zeta|Eta|Theta|Iota|Kappa|Lambda|Mu|Nu|Xi|Omicron|Pi|Rho|Sigma|Tau|Upsilon|Phi|Chi|Psi|Omega|varGamma|varDelta|varTheta|varLambda|varXi|varPi|varSigma|varUpsilon|varPhi|varPsi|varOmega|alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega|varepsilon|varkappa|vartheta|thetasym|varpi|varrho|varsigma|varphi|digamma)  /, '\\$1 ')
          // arity 1
          .replace(/(sqrt)  /,  '\\$1{}')
          // arity 2
          .replace(/(frac|binom)  /,  '\\$1{}{}');
        input.value = sPrime;
        var s = input.value;
        // use s1, s2, and s3 to align equals sign among all lines in a textarea
        var s1 = s.replace(/\n/g, '\\\\ ');
        var s2 = s1.replace(/\ =/g, '&='); // TODO only align equals if first char of line?
        var s3 = "\\begin{aligned}" + s2 + "\\end{aligned}";
        try {
          var l = katex.renderToString(s3, output);
          input.style.backgroundColor = '#FFF';
          input.style.border = '';
          output.innerHTML = l;
        } catch (e) {
          input.style.backgroundColor = '#FEE';
          input.style.border = '3px solid red';
        }
      }
    |]

postPersonR :: Handler Html
postPersonR = do
  ((result, widget), enctype) <- runFormPost personForm
  case result of
    FormSuccess person -> setSession "person" (T.pack $ show person) >> 
      defaultPage [whamlet|
        <p>Name: #{pName person}
        $maybe p <- pAge person
          <p>Age: #{p}
        $nothing
          <p>Age: N/A
        <p>Username: #{pUsername person}
      |]
    _ -> setMessage "Password too short" >> redirect HomeR

getTeacherR :: Handler Text
getTeacherR = return $ T.pack "Teacher's Page"

getJsonR = return $ object ["teacher" .= "Ahson"] -- requires -Xextendeddefaultrules

-- defining my own newtype for higher assurance of correctness
newtype BlogId = BlogId Int
  deriving (Show, Eq, Read)

instance PathPiece BlogId where
  toPathPiece (BlogId id) = T.pack $ show id
  fromPathPiece s =
    case reads $ T.unpack s of
      (id, ""):_
        | id < 0 -> Nothing
        | otherwise -> Just $ BlogId id
      [] -> Nothing

getBlogIdR :: BlogId -> Handler Html
getBlogIdR (BlogId id) = defaultPage [whamlet|<h2>Here is Blog ##{id}.|]

postBlogIdR :: BlogId -> Handler Html
postBlogIdR (BlogId id) = defaultPage [whamlet|<h2>You have uploaded Blog ##{id}.|]

deleteBlogIdR :: BlogId -> Handler Html
deleteBlogIdR (BlogId id) = defaultPage [whamlet|<h2>You have deleted Blog ##{id}.|]

--getMathQuillCssR :: Handler Text
--getMathQuillCssR = sendFile typeCss "static/css/mathquill.css"

--------------------------------------------------------------------------------
-- Define how all pages will look like
defaultPage page = defaultLayout $ do
  toWidgetHead
    [hamlet|
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.css" integrity="sha384-zB1R0rpPzHqg7Kpt0Aljp8JPLqbXI3bhnPWROx27a9N0Ll6ZP/+DiW/UqRcLbRjq" crossorigin="anonymous">

    <!-- The loading of KaTeX is deferred to speed up page rendering -->
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.js" integrity="sha384-y23I5Q6l+B6vatafAwxRu/0oK/79VlbSz7Q9aiSZUvyWYIYsd+qj+o24G5ZU2zJz" crossorigin="anonymous">

    <!-- To automatically render math in text elements, include the auto-render extension: -->
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous" onload="renderMathInElement(document.body);">
    |]
  --addStylesheet MathQuillCssR
  --addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"
  --addScript MathQuillJsR
  --addScriptRemote "https://polyfill.io/v3/polyfill.min.js?features=es6"
  --addScriptRemote "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
  -- TODO: the above uses mathjax for static rendering and mathquill for
  -- dynamic rendering for best looks. Check if mathquill renders fine on other
  -- computers. Use KaTeX instead?
  setTitle "Ahson's Website"
  toWidgetHead
    [hamlet|
      <meta name=keywords content="math homework">
      <style>.err {color: red}
    |]
  [whamlet|^{header}|]
  page
  [whamlet|^{footer}|]

header = do
  mMessage <- getMessage
  toWidget [cassius|
    h1
      color: red
      border: 1px solid #000
  |]
  [whamlet|
    <!-- probably add a nav bar here etc. -->
    <h1>This is Ahson's Website!
    $maybe message <- mMessage
      <p .err>#{message}
    $nothing
      <p>No messages pending.
  |]

footer = do
  year <- liftIO getCurrentYear
  [whamlet|
  <footer style=background:#eee>
    <h3>Footer
    $forall route <- [HomeR, TeacherR, JsonR, MathR] 
      $with sr <- show route
        $with dr <- (take ((length sr) - 1) sr) ++ " Page"
          <a href=@{route}>#{T.pack $ dr}
          <br>
    <p>&#127279; (Copyleft) Hassan Shabbir #{T.pack $ show $ year}
  |]

getCurrentYear :: IO Int
getCurrentYear = do
  now <- getCurrentTime
  let today = utctDay now
  let (year, _, _) = toGregorian today
  return $ fromInteger year

--------------------------------------------------------------------------------
-- NOTES:
--
-- Route Definition:
-- /+ for many, !/foo/bar for allow override
-- name of route becomes constructor in eg. urls
-- leaving off GET will cause all request types to be valid with function handleFooR
-- for subsites: /auth AuthR Auth getAuth
-- route routeR (constructor to get subroutes) predefined
--
-- Html, Css, Js:
-- * Hamlet Html syntax:
-- [hamlet|...|]
-- [whamlet|...|] does not require the toWidget function to be called on it
-- [shamlet|...|] 
--   is simplified hamlet and external file is loaded using shamletFile
--   only embedding of the same type allowed, otherwise not
--
-- #{} variable interpolation
-- ^{} widget (of the same type) interpolation
-- *{} html tag attributes [(key,val)] evaluator
-- _{} html internationalization evaluator
-- @{} url interpolation
-- @?{} url interpolation with query string
--      ie. @?{(SomePage, [("a", "b"), ("c", "d")])}
--      := /somepage?a=b&c=d
--
-- <p>Foo <b>Bar</b> Baz</p> as:
-- <p>Foo #
--   <b>Bar
--   \ Baz
--
-- $if, $elseif, $else
-- $maybe a <- maybeA; $nothing
-- $forall a <- as
-- $case foo; $of Left bar; $of Right baz
-- $with foo <- longExpr
--
-- * Cassius Css syntax:
-- [cassius|...|]
-- 
-- @variable: val; to declare a variable
-- #banner
--   border: 1px solid #{variable}
--   background-image: url(@{BannerImageR})
--
-- * Julius JavaScript syntax:
-- variable interpolation only
--
-- Call Shakespearean languages:
--   quasiquotes: [lang|...|]
--   external file:
--     import Text.Cassius (CssUrl, cassiusFile, renderCss)
--     template :: CssUrl HelloWorld
--     template = $(cassiusFile "myCss.cassius")
--     (may need to use something like renderCss $ template render)
--
--------------------------------------------------------------------------------
-- Widget Methods:
-- toWidget, toWidgetHead and toWidgetBody place the widget in the correct location
-- setTitle
-- addStylesheet, addStylesheetRemote, addScript, addScriptRemote
-- provide strings for the remote functions
--
-- can convert Widget to title, head, body using pageTitle, pageHead and pageBody
-- functions on the Widget
-- or can convert to pageContent using 
-- PageContent title headTags bodyTags <- widgetToPageContent content
--
-- instance Yesod HelloWorld where
  -- defaultLayout = defaultPage
  -- switches to my personal style of pages for all pages by default
--
-- app <- getYesod allows you to ask about the foundation values
-- YesodRequest datatype holds information about the current request such as cookies
--
-- shortcircuit handler functions: 
-- redirect urlR, redirectWith, 
-- notFound, permissionDenied errMsg, invalidArgs [text], sendFile, sendResponse
-- setCookie, deleteCookie, addHeader,
--
-- cookie is encrypted using the client-session-key.aes file
--
--------------------------------------------------------------------------------
-- Sessions:
-- lookupSession, getSession, setSession, deleteSession, 
-- setMessage (it stores for one use), getMessage (deletes previous value)
--
-- ultimate destination helps redirect the user to the correct page if they, for
-- eg. have not logged in, and then take them back to where they arrived
--
-- setUltDest urlR, setUltDestCurrent, setUltDestReferer
-- redirectUltDest defaultR
--
--------------------------------------------------------------------------------
-- Persistent (Db):
-- SQL 	                             | Persistent
-- Datatypes (VARCHAR, INTEGER, etc) | PersistValue
-- Column                            | PersistField
-- Table                             | PersistEntity
--
-- personId <- insert $ Person "John" 23 -- name, age; returns id for inserted row
-- person <- get pId -- using the id above, get the complete row
-- liftIO $ print person
--
-- runSqlite ":memory:" runs the database in memory
--
-- UniqueFooBar foo bar -- causes foo and bar field to be unique together;
-- only this line starts capital; can be named anything with capital letter
-- ufb <- getBy $ UniqueFooBar foo bar -- can be used to uniquely get row from db
--
-- can also use get404 to find value in db, and if it doesn't exist, give a 404
-- also getBy404
--
-- select functions: selectList, selectFirst
-- conduit: selectSource, selectKeys
--
-- selectList: first arg Filters, second arg SelectOpts (sorting, limiting, ofsetting)
-- people <- selectList [PersonAge >. 25, PersonAge <=. 30] []
-- =.   update value
-- !=.  for not equals
-- +=.  add to current and update val
-- -=.  -=
-- *=.  *=
-- /=.  for divide and set
-- <-.  is member
-- /<-. is not member
-- ||.  or
--
-- SelectOpts: Asc colName, Desc colName, LimitTo int, OffsetBy int
--
-- update personId [PersonAge =. 24]
-- updateWhere [PersonName ==. "John"] [PersonAge +=. 1]
-- replace personId $ Person "Jane" 40
-- delete personId
-- deleteBy $ UniqueFooBar foo bar
-- deleteWhere [PersonName ==. "Jane"]
-- deleteWhere ([] :: [Filter Person]) -- truncates table
--
-- set columns default value: 
-- created UTCTime default=CURRENT_TIME -- column still required
-- 
-- many to many supported, using a join table
--
-- derive an enumerated column; must be kept in another file:
-- data Employment = Employed | Unemployed | Retired
--   deriving (Show, Read, Eq)
-- derivePersistField "Employment"
--
-- run raw sql using persistent:
-- import qualified Data.Conduit.List as CL
-- let sql = "SELECT name FROM Person WHERE name LIKE '%Snoyman'"
--   rawQuery sql [] $$ CL.mapM_ (liftIO . print)
--
--------------------------------------------------------------------------------
-- Authentication and Authorization:
-- Client ID: 651837639044-vj9quhrfhcn1742hujini9i9uib3ndah.apps.googleusercontent.com
-- Client secret: w7wgbXVSLQ9fmMKNBk9Sc6Jo
-- Creation date: April 20, 2020 at 10:30:19 PM GMT-4
--
-- functions for querying authentication info:
-- maybeAuthId, requireAuthId
-- maybeAuth,   requireAuth
-- 
-- can force ssl using info from sessions chapter
--
--------------------------------------------------------------------------------
-- Deploying:
-- stack clean -- to remove unnessary flies
-- stack build -- generates a file in dist/build/myapp/myapp ignore this step?
-- stack install -- generates files in .local/bin, create yesod app basic
--
-- push the following files: 
-- executable file, config folder (including client_session_key.aes), static folder
--
--------------------------------------------------------------------------------
-- Extra Comments not really needed:
-- 
--import Data.Char (toUpper)
--import Text.Hamlet (shamlet)
--import Text.Blaze.Html.Renderer.String (renderHtml)
--data HelloWorld = HelloWorld ConnectionPool { httpManager :: Manager }
{- alternative way for in-memory db, plus some example sql commands
runSqlite ":memory:" $ do
  -- warp 3000 HelloWorld 
  runMigration migrateAll

  johnId <- insert $ Author "John Doe" $ Just 35
  john <- get johnId
  liftIO $ print john

  liftIO $ warp 3000 $ HelloWorld pool
-}

--footer :: HtmlUrl (Route HelloWorld) -- give this type to widgets
--     <p>
--      When \(a \ne 0\), there are two solutions to \(ax^2 + bx + c = 0\) and they are \[x = {-b \pm \sqrt{b^2-4ac} \over 2a}.\]
--    <p>Solve #
--      <span #question0>\(x^2 = 100\)
--      \.
--    <p>\(x = \)
--      <span #answer0 style=margin:8px>-}
--      {-//let $ = document.querySelector.bind(document);
--      var MQ = MathQuill.getInterface(2);
--      var answer0 = document.getElementById('answer0');
--      var answerField = MQ.MathField(answer0);-}
