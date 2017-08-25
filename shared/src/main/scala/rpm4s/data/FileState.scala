package rpm4s.data

//TODO: implement this
class FileState {
  val RPMFILE_STATE_MISSING = -1 /* used for unavailable data */
  val RPMFILE_STATE_NORMAL = 0
  val RPMFILE_STATE_REPLACED = 1
  val RPMFILE_STATE_NOTINSTALLED = 2
  val RPMFILE_STATE_NETSHARED = 3
  val RPMFILE_STATE_WRONGCOLOR = 4
}
