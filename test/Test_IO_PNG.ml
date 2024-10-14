open Alcotest
module Diff = Odiff.Diff.MakeDiff (Png.IO) (Png.IO)

let load_image path =
  match Png.IO.loadImage path with
  | exception ex ->
      fail
        (Printf.sprintf "Failed to load image: %s\nError: %s" path
           (Printexc.to_string ex))
  | img -> img

let () =
  run "IO"
    [
      ( "PNG",
        [
          test_case "finds difference between 2 images" `Quick (fun () ->
              let img1 = load_image "test-images/png/orange.png" in
              let img2 = load_image "test-images/png/orange_changed.png" in
              let diffPixels, diffPercentage, _ =
                Diff.compare img1 img2 ()
              in
              check int "diffPixels" 1366 diffPixels;
              check (float 0.1) "diffPercentage" 1.14 diffPercentage);
          test_case "Diff of mask and no mask are equal" `Quick (fun () ->
              let img1 = load_image "test-images/png/orange.png" in
              let img2 = load_image "test-images/png/orange_changed.png" in
              let diffPixels, diffPercentage, _ =
                Diff.compare img1 img2 ()
              in
              let img1 = load_image "test-images/png/orange.png" in
              let img2 = load_image "test-images/png/orange_changed.png" in
              let diffPixelsMask, diffPercentageMask, _ =
                Diff.compare img1 img2 ()
              in
              check int "diffPixels" diffPixels diffPixelsMask;
              check (float 0.001) "diffPercentage" diffPercentage
                diffPercentageMask);
          test_case "Creates correct diff output image" `Quick (fun () ->
              let img1 = load_image "test-images/png/orange.png" in
              let img2 = load_image "test-images/png/orange_changed.png" in
              let diffOutput = img1 in
              let _, _, _ = Diff.compare img1 img2 ~diffOutput () in
              let originalDiff = load_image "test-images/png/orange_diff.png" in
              let diffMaskOfDiff = originalDiff in
              let diffOfDiffPixels, diffOfDiffPercentage, _ =
                Diff.compare originalDiff diffOutput ~diffOutput:diffMaskOfDiff ()
              in
              if diffOfDiffPixels > 0 then (
                Png.IO.saveImage diffOutput "test-images/png/diff-output.png";
                Png.IO.saveImage diffMaskOfDiff
                  "test-images/png/diff-of-diff.png");
              check int "diffOfDiffPixels" 0 diffOfDiffPixels;
              check (float 0.001) "diffOfDiffPercentage" 0.0
                diffOfDiffPercentage);
          test_case "Correctly handles different encodings of transparency"
            `Quick (fun () ->
              let img1 = load_image "test-images/png/extreme-alpha.png" in
              let img2 = load_image "test-images/png/extreme-alpha-1.png" in
              let diffPixels, _, _ = Diff.compare img1 img2 () in
              check int "diffPixels" 0 diffPixels);
        ] );
    ]
