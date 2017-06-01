## enum with automatic enumeration
## source: https://stackoverflow.com/a/1695250/2725810
def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    return type('Enum', (), enums)
