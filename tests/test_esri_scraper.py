import contextlib
import io
from requests import Session
import unittest
from unittest.mock import Mock, patch
from esri_out_of_the_way import ESRIInTheWayException, ESRIScraper


class ESRIResponse:
    def __init__(self, status_code, content):
        self.status_code = status_code
        self.content = content


class TestESRIScraper(unittest.TestCase):
    @classmethod
    def setUpClass(self) -> None:
        self.es_obj = ESRIScraper()
    
    def test_get_webmaps__shouldnt_run(self) -> None:
        # The function should not run if the appid is not a 32 character string
        not_a_32_char_string = "Derpaderpaderp"
        err_msg = f"'{not_a_32_char_string}' is not a 32 character hex string"
        with self.assertRaisesRegex(ESRIInTheWayException, err_msg) as e:
            self.es_obj.get_webmaps(not_a_32_char_string)

    def test_get_webmaps__dont_conform_to_key_expectation(self):
        # The JSON returned by ESRI may not have the webmap where we think
        # it is. In these cases, we'll want to notify the user of this and
        # return the JSON. The user can then look at that and hopefully the
        # webmap will be there.
        appid = 'f8473d6f257b4e6bab4d42c343e3901d'
        stdout = io.StringIO() # This is where captured output goes
        # A fake response, that is 200 - OK
        response = ESRIResponse(
            status_code=200, content=b'{"values": {"thang": 3}}'
        )
        # With the patch function, `mock_responses` will replace
        # the requests library's `Session`.`get()` method with our mock
        mock_response = Mock()
        mock_response.return_value = response
        with patch.object(Session, 'get', mock_response):
            with contextlib.redirect_stdout(stdout):
                result = self.es_obj.get_webmaps(appid)
                self.assertIn("key path does not conform", stdout.getvalue())
                self.assertEqual(result['values']['thang'], 3)


if __name__ == '__main__':
    unittest.main()