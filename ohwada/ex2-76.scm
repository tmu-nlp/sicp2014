; generic operations with explicit dispatch
; 新しい型 → 各汎用 selector に、type-tag の判別を行う手続き (polar? などの) を追加する 
; 新しい演算 → 新しい演算手続きをつくる

; data-directed style
; 新しい型 → 新しいインストールパッケージをつくる
; 新しい演算 → 各パッケージに手続きを追加する

; message-passing-style
; 新しい型 → 新しいデータオブジェクト (直交座標数などの) を追加する
; 新しい演算 → 各データオブジェクトに手続きを追加する


; 従って適している実装は...
; 型がどんどん追加されるシステム → data-directed style か message-passing-style
; 演算がどんどん追加されるシステム → generic operations with explicit dispatch
