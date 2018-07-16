module Castle where

import Dice
import Area

type Country = String

data Castle = Castle
    { csArea :: Area
    , csCountry :: Country
    , csName :: Name
    , csPower :: [Int]
    }

instance Show Castle where
    show castle =
        "<" ++ (csName castle) ++ ">:" ++
        (show $ csArea castle) ++ ":" ++
        (csCountry castle) ++ ":" ++
        (show $ csPower castle)

castleDeck :: [Castle]
castleDeck = 
    [ Castle (East 1) "三河" "岡崎城" [2]
    , Castle (East 1) "尾張" "名古屋城" [4]
    , Castle (East 1) "美濃" "稲葉山城" [5]
    , Castle (East 2) "甲斐" "躑躅ヶ崎館" [5]
    , Castle (East 2) "相模" "小田原城" [2, 3, 4]
    , Castle (East 2) "駿河" "駿府城" [3]
    , Castle (East 3) "武蔵" "江戸城" [3, 4]
    , Castle (East 3) "越後" "春日山城" [2, 3]
    , Castle (East 4) "出羽" "米沢城" [3]
    , Castle (East 4) "蝦夷" "五稜郭" [5]
    , Castle (East 5) "桜面戸" "金門橋" [1, 1]
    , Castle (East 6) "紐育" "萬八端島" [3, 4, 5]
    , Castle (West 1) "大和" "東大寺" [1, 1]
    , Castle (West 1) "山城" "二条城" [1, 1, 1]
    , Castle (West 1) "河内" "石山本願寺" [5]
    , Castle (West 2) "備前" "岡山城" [2]
    , Castle (West 2) "播磨" "姫路城" [3, 4]
    , Castle (West 3) "出雲" "出雲大社" [1, 1]
    , Castle (West 3) "土佐" "高知城" [5]
    , Castle (West 4) "琉球" "首里城" [2]
    , Castle (West 4) "薩摩" "鹿児島城" [5]
    , Castle (West 5) "唐" "万里の長城" [6]
    , Castle (West 5) "天竺" "東インド会社" [2, 3]
    , Castle (West 6) "羅馬" "法王庁" [1, 1, 1]
    ]
