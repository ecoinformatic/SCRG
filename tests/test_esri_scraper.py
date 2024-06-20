import unittest
from esri_out_of_the_way import ESRIInTheWayException, ESRIScraper


class TestESRIScraper(unittest.TestCase):
    @classmethod
    def setUpClass(self) -> None:
        self.es_obj = ESRIScraper()
    
    def test_get_webmaps__shouldnt_run(self) -> None:
        # The test should not run if the appid is not a 32 character string
        not_a_32_char_string = "Derpaderpaderp"
        err_msg = f"'{not_a_32_char_string}' is not a 32 character hex string"
        with self.assertRaisesRegex(ESRIInTheWayException, err_msg) as e:
            self.es_obj.get_webmaps(not_a_32_char_string)


if __name__ == '__main__':
    unittest.main()