guard :haskell, all_on_start: true, all_on_pass: true, ghci_options: ["-DTEST", "-ignore-dot-ghci"] do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
end
