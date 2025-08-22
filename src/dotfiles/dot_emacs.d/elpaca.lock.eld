((anzu :source "elpaca-menu-lock-file" :recipe
       (:package "anzu" :fetcher github :repo "emacsorphanage/anzu"
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :wait nil :ref
                 "21cb5ab2295614372cb9f1a21429381e49a6255f"))
 (atom-one-dark-theme :source "elpaca-menu-lock-file" :recipe
                      (:package "atom-one-dark-theme" :fetcher github
                                :repo
                                "jonathanchu/atom-one-dark-theme"
                                :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file"
                                :protocol https :inherit t :depth
                                treeless :wait nil :ref
                                "b34b62e85593812b55ee552a1cb0eecfb04767bb"))
 (auctex :source "elpaca-menu-lock-file" :recipe
         (:package "auctex" :repo
                   ("https://git.savannah.gnu.org/git/auctex.git"
                    . "auctex")
                   :branch "main" :files ("*" (:exclude ".git"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :wait nil :ref
                   "9eac86706d180ff542efeed8c6d4407fdfa6bbf4"))
 (auctex-latexmk :source "elpaca-menu-lock-file" :recipe
                 (:package "auctex-latexmk" :fetcher github :repo
                           "emacsmirror/auctex-latexmk" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :protocol
                           https :inherit t :depth treeless :wait nil
                           :ref
                           "b00a95e6b34c94987fda5a57c20cfe2f064b1c7a"))
 (company :source "elpaca-menu-lock-file" :recipe
          (:package "company" :fetcher github :repo
                    "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small"
                                "doc/images/small/*.png"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :wait nil :ref
                    "9c273fc7c1a9dd69ccf508589211c4f8bd0e0765"))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo
                   ("https://github.com/emacs-compat/compat"
                    . "compat")
                   :files ("*" (:exclude ".git")) :source
                   "elpaca-menu-lock-file" :protocol https :inherit t
                   :depth treeless :ref
                   "a4c0408eafc3958af869727874b715ab6ca23b68" :wait
                   nil))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source
                 "elpaca-menu-lock-file" :protocol https :inherit t
                 :depth treeless :wait nil :ref
                 "1de9dcb83eacfb162b6d9a118a4770b1281bcd84"))
 (ebib :source "elpaca-menu-lock-file" :recipe
       (:package "ebib" :fetcher github :repo "joostkremers/ebib"
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :wait nil :ref
                 "cc81455e7c87182d4226b878bef1d000f0866953"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth
            treeless :repo "https://github.com/progfolio/elpaca.git"
            :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca--activate-package) :package "elpaca"
            :ref "10e65441f34253254272aeacd300574b576894a2"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main
                               "extensions/elpaca-use-package.el"
                               :build (:not elpaca--compile-info)
                               :source "elpaca-menu-lock-file"
                               :protocol https :inherit t :depth
                               treeless :ref
                               "10e65441f34253254272aeacd300574b576894a2"))
 (ess :source "elpaca-menu-lock-file" :recipe
      (:package "ess" :repo "emacs-ess/ESS" :fetcher github :files
                ("lisp/*.el" "doc/ess.texi" ("etc" "etc/*")
                 ("obsolete" "lisp/obsolete/*") (:exclude "etc/other"))
                :source "elpaca-menu-lock-file" :protocol https
                :inherit t :depth treeless :wait nil :ref
                "d60c13a6a347ea7a91ea3408bb464cff0ab4fef6"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit
              t :depth treeless :wait nil :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :wait nil :ref
                     "5217e0e02f487d70e3d418e5e6ce884fc9711511"))
 (forth-mode :source "elpaca-menu-lock-file" :recipe
             (:package "forth-mode" :fetcher github :repo
                       "larsbrinkhoff/forth-mode" :files
                       (:defaults "backend" (:exclude "build.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :wait nil :ref
                       "59c5ea89ca7593bd49cdde6caefa0893a8780105"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher
                    github :files ("go-mode.el") :source
                    "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :wait nil :ref
                    "602d73e22646b1b98b2eb97927fd426c0d1d2f92"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :wait nil :ref
                    "82eba6b8f7b5a4cbcf22436d5c5b88fb3134f57e"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :protocol https
               :inherit t :depth treeless :wait nil :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (julia-mode :source "elpaca-menu-lock-file" :recipe
             (:package "julia-mode" :repo
                       "JuliaEditorSupport/julia-emacs" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :wait nil :ref
                       "09897a8cbab48adaacdef6f852d7cebd3945a645"))
 (just-mode :source "elpaca-menu-lock-file" :recipe
            (:package "just-mode" :repo "leon-barrett/just-mode.el"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :wait nil :ref
                      "4c0df4cc4b8798f1a7e99fb78b79c4bf7eec12c1"))
 (latex-preview-pane :source "elpaca-menu-lock-file" :recipe
                     (:package "latex-preview-pane" :fetcher github
                               :repo "jsinglet/latex-preview-pane"
                               :files ("*.el" "*.txt") :source
                               "elpaca-menu-lock-file" :protocol https
                               :inherit t :depth treeless :wait nil
                               :ref
                               "5297668a89996b50b2b62f99cba01cc544dbed2e"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :wait nil :ref
                  "5990ae7b5cba3425252f64b9377352c9a01690d5"))
 (llvm-mode :source "elpaca-menu-lock-file" :recipe
            (:source "elpaca-menu-lock-file" :protocol https :inherit
                     t :depth treeless :wait nil :host github :repo
                     "llvm/llvm-project" :files
                     ("llvm/utils/emacs/llvm-mode.el") :package
                     "llvm-mode" :ref
                     "be64ca912371ae62002aee3429de058052451d72"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher
                     github :files (:defaults "clients/*.el") :source
                     "elpaca-menu-lock-file" :protocol https :inherit
                     t :depth treeless :wait nil :ref
                     "9b104105adecf637188f4e3bf5cc228bda12374a"))
 (lsp-ui :source "elpaca-menu-lock-file" :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github
                   :files (:defaults "lsp-ui-doc.html" "resources")
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :wait nil :ref
                   "072bb29152038518c2478813b82c8d04d07df84c"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "elpaca-menu-lock-file" :protocol
               https :inherit t :depth treeless :wait nil :ref
               "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" "magit-pkg.el"
                   (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :wait nil :ref
                  "cc40a7b1f9d2f343779170591bfbd2c4cecae998"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :wait nil
                          :ref
                          "cc40a7b1f9d2f343779170591bfbd2c4cecae998"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :wait nil
                          :ref
                          "6102ac5b7301b4c4fc0262d9c6516693d5a33f2b"))
 (move-text :source "elpaca-menu-lock-file" :recipe
            (:package "move-text" :fetcher github :repo
                      "emacsfodder/move-text" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :wait nil :ref
                      "2a8ebefeb0b363681e9562847eca3fd66e090d70"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                             "magnars/multiple-cursors.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :wait
                             nil :ref
                             "c870c18462461df19382ecd2f9374c8b902cd804"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo
                         "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :wait nil
                         :ref
                         "bbcbdcdce968a0ac7c293314e1aa7dfdda63a30c"))
 (olivetti :source "elpaca-menu-lock-file" :recipe
           (:package "olivetti" :fetcher github :repo "rnkn/olivetti"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :wait nil :ref
                     "845eb7a95a3ca3325f1120c654d761b91683f598"))
 (page-break-lines :source "elpaca-menu-lock-file" :recipe
                   (:package "page-break-lines" :fetcher github :repo
                             "purcell/page-break-lines" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :wait
                             nil :ref
                             "4a20d4a28a1228c561b151f868611ad75de90e5e"))
 (paredit :source "elpaca-menu-lock-file" :recipe
          (:package "paredit" :fetcher git :url
                    "https://mumble.net/~campbell/git/paredit.git"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :wait nil :ref
                    "af075775af91f2dbc63b915d762b4aec092946c4"))
 (parsebib :source "elpaca-menu-lock-file" :recipe
           (:package "parsebib" :fetcher github :repo
                     "joostkremers/parsebib" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :wait nil :ref
                     "05fff935749e6ca29ec81fdf0163e95ad2aa6bc3"))
 (rich-minority :source "elpaca-menu-lock-file" :recipe
                (:package "rich-minority" :repo
                          "Malabarba/rich-minority" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :wait nil
                          :ref
                          "77cf5ec620aaef18385d2e1d2dad05b4f63dad95"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :wait nil :ref
                      "3bd0863f28414e629ca15ad2852a429a126226c5"))
 (rustic :source "elpaca-menu-lock-file" :recipe
         (:package "rustic" :repo "emacs-rustic/rustic" :fetcher
                   github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :wait nil :ref
                   "2f3c5ffd75377741eaade356aa0f9a8bf0604d9b"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit
              t :depth treeless :wait nil :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (smart-mode-line :source "elpaca-menu-lock-file" :recipe
                  (:package "smart-mode-line" :repo
                            "Malabarba/smart-mode-line" :fetcher
                            github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :wait nil
                            :ref
                            "bbed708eb8393697e01ab2474dfb54d7c5ea7905"))
 (smart-mode-line-atom-one-dark-theme :source "elpaca-menu-lock-file"
                                      :recipe
                                      (:package
                                       "smart-mode-line-atom-one-dark-theme"
                                       :repo
                                       "daviderestivo/smart-mode-line-atom-one-dark-theme"
                                       :fetcher github :files
                                       ("*.el" "*.el.in" "dir"
                                        "*.info" "*.texi" "*.texinfo"
                                        "doc/dir" "doc/*.info"
                                        "doc/*.texi" "doc/*.texinfo"
                                        "lisp/*.el" "docs/dir"
                                        "docs/*.info" "docs/*.texi"
                                        "docs/*.texinfo"
                                        (:exclude ".dir-locals.el"
                                                  "test.el" "tests.el"
                                                  "*-test.el"
                                                  "*-tests.el"
                                                  "LICENSE" "README*"
                                                  "*-pkg.el"))
                                       :source "elpaca-menu-lock-file"
                                       :protocol https :inherit t
                                       :depth treeless :wait nil :ref
                                       "f422b79e7b6e2796b1d5f4143913497383840960"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :files ("*" (:exclude ".git")) :source
                    "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :wait nil :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (splunk-mode :source "elpaca-menu-lock-file" :recipe
              (:package "splunk-mode" :fetcher github :repo
                        "jakewilliami/splunk-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :wait nil
                        :ref
                        "79d7ecc4d79592cb66270730f0b9f362a87f6752"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo
                      "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :wait nil :ref
                      "07a104aa73b5d5e3168ba6e202ebfb60188af6ed"))
 (visual-fill-column :source "elpaca-menu-lock-file" :recipe
                     (:package "visual-fill-column" :fetcher codeberg
                               :repo "joostkremers/visual-fill-column"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file"
                               :protocol https :inherit t :depth
                               treeless :wait nil :ref
                               "e04d3521b6dc2435de4c4a4b9cac5feb194f0d5b"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo
                        "magit/with-editor" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :wait nil
                        :ref
                        "77cb2403158cfea9d8bfb8adad81b84d1d6d7c6a"))
 (writeroom-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "writeroom-mode" :fetcher github :repo
                           "joostkremers/writeroom-mode" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :protocol
                           https :inherit t :depth treeless :wait nil
                           :ref
                           "f4d035e91d20bf1dd3f2857b9cc344f844979a78"))
 (xterm-color :source "elpaca-menu-lock-file" :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :wait nil
                        :ref
                        "2ad407c651e90fff2ea85d17bf074cee2c022912"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet"
                      :fetcher github :files
                      ("yasnippet.el" "snippets") :source
                      "elpaca-menu-lock-file" :protocol https :inherit
                      t :depth treeless :wait nil :ref
                      "fe1f4e0e96ce42d8668920335eb22c3c009dab3e")))
