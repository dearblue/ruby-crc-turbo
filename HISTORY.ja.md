This document is written in Japanese.

# crc-turbo for ruby の更新履歴


## crc-0.4 (TRYOUT)

  * crc-0.4 に追従


## crc-0.3 (平成28年7月31日 日曜日)

  * crc-0.3 に追従して、CRC::BasicCRC と CRC::Generator を CRC に統合

## crc-0.2 (平成28年5月15日 (日))

  * \[FIX\] CRC::Generator#polynomial メソッドが間違った値を返していた問題を修正
  * CRC::Utils.bitreflect128 メソッドの実装
  * CRC::BasicCRC#update メソッドの実装を slice-by-eight から slice-by-16 へと変更
  * CRC ルックアップテーブル領域確保の時機を初回計算時の直前とするように改善

## crc-0.1 (平成28年5月8日 (日))

初版
