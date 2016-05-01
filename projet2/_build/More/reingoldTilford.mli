type interval =
  { left_end : float;
    right_end : float;
  }


val annotate_with_offset :
  'elt BinTree.bintree -> interval list * ('elt * float) BinTree.bintree

val layout :
  'elt BinTree.bintree -> Gg.Box2.t * ('elt * Gg.p2) BinTree.bintree


val image_of_tree :
     ?image_of_node:('elt * Gg.p2 -> Vg.image)
  -> ?image_of_edge:('elt * Gg.p2 -> 'elt * Gg.p2 -> Vg.image)
  -> ('elt * Gg.p2) BinTree.bintree
  -> Vg.image
