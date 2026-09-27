module Analysis : module type of Demo_analysis
module Printer : module type of Lsp_demo_syntax.Syntax_tree_printer
module Language : Language_server.Language
module Server : Language_server.Server
