#|

serializerの実装による。

が、想像をしてみると安全な変更ではないように思う。
1つのアカウントがwithdrawしているときに、もう一つのアカウントが
withdrawをしようとすると、両方とも同じ直列化なので競合するから。

http://community.schemewiki.org/?sicp-ex-3.42
では安全である意見の方が多い。
しかし現在の情報からは実装によるのでオリジナルの方が安全と言える。

|#