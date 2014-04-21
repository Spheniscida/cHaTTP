module MainTest where

import Data.ByteString.Lazy.Char8 as BS
import Chattp.Webapp.Protocol

import Test.HUnit

main = runTestTT tests

tests = TestList [ TestCase create_request_1,
                   TestCase parse_answer_1,
                   TestCase parse_answer_2,
                   TestCase parse_answer_3,
                   TestCase parse_answer_4,
                   TestCase parse_answer_5,
                   TestCase parse_answer_6
                   ]

-- Conf tests

create_request_1 = assertEqual "" (BS.pack "123\nLOGIN\nusr\npwd") (requestToByteString (BrokerRequestMessage 123 (Login (BS.pack "usr") (BS.pack "pwd"))))

parse_answer_1 = assertEqual "parse_answer_1" (Just $ BrokerAnswerMessage 123 (UserLoggedIn OK (Just (BS.pack "abcde")))) (fromEither $ parseAnswer (BS.pack "123\nLGDIN\nOK\nabcde"))
parse_answer_2 = assertEqual "parse_answer_2" (Just $ BrokerAnswerMessage 123 (UserLoggedOut OK)) (fromEither $ parseAnswer (BS.pack "123\nLGDOUT\nOK"))
parse_answer_3 = assertEqual "parse_answer_3" (Just $ BrokerAnswerMessage 123 (MessageAccepted OK)) (fromEither $ parseAnswer (BS.pack "123\nACCMSG\nOK"))
parse_answer_4 = assertEqual "parse_answer_4" (Just $ BrokerAnswerMessage 123 (UserStatus ONLINE)) (fromEither $ parseAnswer (BS.pack "123\nUONL\nY"))
parse_answer_5 = assertEqual "parse_answer_5" (Just $ BrokerAnswerMessage 123 (UserRegistered FAIL)) (fromEither $ parseAnswer (BS.pack "123\nUREGD\nFAIL"))
parse_answer_6 = assertEqual "parse_answer_6" (Just $ BrokerAnswerMessage 123 (UserLoggedIn FAIL Nothing)) (fromEither $ parseAnswer (BS.pack "123\nLGDIN\nFAIL"))

-- Utils

fromEither :: Either a b -> Maybe b
fromEither (Left a) = Nothing
fromEither (Right b) = Just b

