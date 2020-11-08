import unittest

import gtirb


class CFGTest(unittest.TestCase):
    def test_contains(self):
        b1, b2 = gtirb.ProxyBlock(), gtirb.ProxyBlock()
        cfg = gtirb.CFG(
            [gtirb.Edge(b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Branch))]
        )
        self.assertFalse(
            gtirb.Edge(gtirb.ProxyBlock(), gtirb.ProxyBlock()) in cfg
        )
        self.assertFalse(
            gtirb.Edge(b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Fallthrough))
            in cfg
        )
        self.assertTrue(
            gtirb.Edge(b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Branch)) in cfg
        )

    def test_add(self):
        b1, b2 = gtirb.ProxyBlock(), gtirb.ProxyBlock()
        cfg = gtirb.CFG()
        cfg.add(gtirb.Edge(b1, b2))
        cfg.add(gtirb.Edge(b1, b2))
        cfg.add(gtirb.Edge(b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Branch)))
        self.assertEqual(len(cfg), 2)
        self.assertTrue(gtirb.Edge(b1, b2) in cfg)
        self.assertTrue(
            gtirb.Edge(b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Branch)) in cfg
        )

    def test_clear(self):
        cfg = gtirb.CFG(
            [
                gtirb.Edge(gtirb.ProxyBlock(), gtirb.ProxyBlock()),
                gtirb.Edge(
                    gtirb.CodeBlock(offset=0, size=1),
                    gtirb.CodeBlock(offset=1, size=2),
                ),
            ]
        )
        self.assertEqual(len(cfg), 2)

        cfg.clear()
        self.assertEqual(len(cfg), 0)

    def test_discard(self):
        b1, b2 = gtirb.ProxyBlock(), gtirb.CodeBlock(offset=0, size=1)
        cfg = gtirb.CFG(
            [
                gtirb.Edge(b1, b2),
                gtirb.Edge(
                    gtirb.ProxyBlock(), gtirb.CodeBlock(offset=1, size=2)
                ),
            ]
        )
        self.assertEqual(len(cfg), 2)

        cfg.discard(gtirb.Edge(b1, b2))
        self.assertEqual(len(cfg), 1)
        self.assertFalse(gtirb.Edge(b1, b2) in cfg)

        cfg.discard(gtirb.Edge(b1, b2))
        self.assertEqual(len(cfg), 1)
        self.assertFalse(gtirb.Edge(b1, b2) in cfg)

    def test_out_edges(self):
        b1, b2, b3 = gtirb.ProxyBlock(), gtirb.ProxyBlock(), gtirb.ProxyBlock()
        b4 = gtirb.CodeBlock(offset=0, size=1)
        cfg = gtirb.CFG(
            [
                gtirb.Edge(
                    b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Fallthrough)
                ),
                gtirb.Edge(b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Branch)),
                gtirb.Edge(b3, b2),
            ]
        )
        self.assertEqual(sum(1 for _ in cfg.out_edges(b1)), 2)
        self.assertEqual(sum(1 for _ in cfg.out_edges(b2)), 0)
        self.assertEqual(sum(1 for _ in cfg.out_edges(b3)), 1)
        self.assertEqual(sum(1 for _ in cfg.out_edges(b4)), 0)

    def test_in_edges(self):
        b1, b2, b3 = gtirb.ProxyBlock(), gtirb.ProxyBlock(), gtirb.ProxyBlock()
        b4 = gtirb.CodeBlock(offset=0, size=1)
        cfg = gtirb.CFG(
            [
                gtirb.Edge(
                    b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Fallthrough)
                ),
                gtirb.Edge(b3, b2, gtirb.Edge.Label(gtirb.Edge.Type.Branch)),
                gtirb.Edge(b1, b3),
            ]
        )
        self.assertEqual(sum(1 for _ in cfg.in_edges(b1)), 0)
        self.assertEqual(sum(1 for _ in cfg.in_edges(b2)), 2)
        self.assertEqual(sum(1 for _ in cfg.in_edges(b3)), 1)
        self.assertEqual(sum(1 for _ in cfg.in_edges(b4)), 0)

    def test_nx(self):
        b1, b2 = gtirb.ProxyBlock(), gtirb.ProxyBlock()
        cfg = gtirb.CFG(
            [gtirb.Edge(b1, b2, gtirb.Edge.Label(gtirb.Edge.Type.Call))]
        )
        for n1, n2, lab in cfg.nx().edges(data="label"):
            self.assertEqual(n1, b1)
            self.assertEqual(n2, b2)
            self.assertEqual(lab, gtirb.Edge.Label(gtirb.Edge.Type.Call))
