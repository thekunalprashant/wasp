module Wasp.Node.Version
  ( getAndCheckNodeVersion,
    VersionCheckResult (..),
    oldestWaspSupportedNodeVersion,
  )
where

import Data.Conduit.Process.Typed (ExitCode (..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified System.Process as P
import Text.Read (readMaybe)
import qualified Text.Regex.TDFA as R
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (indent)

-- | Wasp supports any node version equal or greater to this version.
-- | We usually keep this one equal to the latest LTS.
oldestWaspSupportedNodeVersion :: SV.Version
oldestWaspSupportedNodeVersion = SV.Version 20 8 1

data VersionCheckResult
  = VersionCheckFail !ErrorMessage
  | VersionCheckSuccess !(Maybe WarningMessage) !SV.Version

type WarningMessage = String

type ErrorMessage = String

-- | Gets the installed node version, if any is installed, and checks that it
-- meets Wasp's version requirement.
getAndCheckNodeVersion :: IO VersionCheckResult
getAndCheckNodeVersion =
  getNodeVersion >>= \case
    Left errorMsg -> return $ VersionCheckFail errorMsg
    Right userNodeVersion ->
      return $
        if SV.isVersionInRange userNodeVersion $ SV.Range [SV.gte oldestWaspSupportedNodeVersion]
          then VersionCheckSuccess Nothing userNodeVersion
          else VersionCheckFail $ makeNodeVersionMismatchMessage userNodeVersion

makeNodeVersionMismatchMessage :: SV.Version -> String
makeNodeVersionMismatchMessage nodeVersion =
  unlines
    [ unwords
        [ "Your Node version does not meet Wasp's requirements!",
          "You are running Node " ++ show nodeVersion ++ "."
        ],
      waspNodeRequirementMessage
    ]

waspNodeRequirementMessage :: String
waspNodeRequirementMessage =
  unwords
    [ "Wasp requires Node >=" ++ show oldestWaspSupportedNodeVersion ++ " to be installed and in PATH.",
      "Check Wasp documentation for more details: https://wasp-lang.dev/docs/quick-start#requirements."
    ]

-- | Gets the installed node version, if any is installed, and returns it.
--
-- Returns a string representing the error condition if node's version could
-- not be found.
getNodeVersion :: IO (Either ErrorMessage SV.Version)
getNodeVersion = do
  -- Node result is one of:
  -- 1. @Left processError@, when an error occurs trying to run the process
  -- 2. @Right (ExitCode, stdout, stderr)@, when the node process runs and terminates
  nodeResult <-
    (Right <$> P.readProcessWithExitCode "node" ["--version"] "")
      `catchIOError` ( \e ->
                         if isDoesNotExistError e
                           then return $ Left nodeNotFoundMessage
                           else return $ Left $ makeNodeUnknownErrorMessage e
                     )
  return $ case nodeResult of
    Left procErr ->
      Left
        ( unlines
            [ procErr,
              waspNodeRequirementMessage
            ]
        )
    Right (ExitFailure code, _, stderr) ->
      Left
        ( unlines
            [ "Running `node --version` failed (exit code " ++ show code ++ "):",
              indent 2 stderr,
              waspNodeRequirementMessage
            ]
        )
    Right (ExitSuccess, stdout, _) -> case parseNodeVersion stdout of
      Nothing ->
        Left
          ( "Wasp failed to parse node version."
              ++ " This is most likely a bug in Wasp, please file an issue."
          )
      Just version -> Right version

parseNodeVersion :: String -> Maybe SV.Version
parseNodeVersion nodeVersionStr =
  case nodeVersionStr R.=~ ("v([^\\.]+).([^\\.]+).(.+)" :: String) of
    ((_, _, _, [majorStr, minorStr, patchStr]) :: (String, String, String, [String])) -> do
      mjr <- readMaybe majorStr
      mnr <- readMaybe minorStr
      ptc <- readMaybe patchStr
      return $ SV.Version mjr mnr ptc
    _ -> Nothing

nodeNotFoundMessage :: String
nodeNotFoundMessage = "`node` command not found!"

makeNodeUnknownErrorMessage :: IOError -> String
makeNodeUnknownErrorMessage err =
  unlines
    [ "An unknown error occured while trying to run `node --version`:",
      indent 2 $ show err
    ]
