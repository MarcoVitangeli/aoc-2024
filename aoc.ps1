$day, $part = $args[0] -split '\.'

Push-Location "day$day"

ghc "p$part"

if ($LASTEXITCODE -eq 0) {
    & "./p$part.exe"
}

Pop-Location

