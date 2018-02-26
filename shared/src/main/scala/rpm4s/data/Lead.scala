package rpm4s.data

/**
 * This header is deprecated (except for signatureType) and not really
 * used anymore but each rpm still requires it at least to be present.
 *
 * @param major
 * @param minor
 * @param tpe
 * @param archnum
 * @param name
 * @param osnum
 * @param signatureType  type of [[rpm4s.codecs.HeaderTag.HeaderSignatures]]
 */
case class Lead(
  major: Int,
  minor: Int,
  tpe: RPMType,
  //archnum: Architecture,
  archnum: Short,
  name: String,
  osnum: OS,
  signatureType: Int
)
