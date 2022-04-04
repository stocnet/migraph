# Tests for the blockmodel family of functions

# 1. Testing blockmodel_concor()

# test_that("concor works on one-mode networks", {
#   expect_equal(blockmodel_concor(mpn_elite_mex)$membership[1:11], 
#                c(1,1,1,1,1,1,1,1,1,1,1))
#   expect_equal(blockmodel_concor(mpn_elite_mex, p = 2)$membership[1:11], 
#                c(1,1,1,2,1,1,1,1,1,2,2))
#   expect_equal(blockmodel_concor(mpn_elite_mex, p = 3)$membership[1:11], 
#                c(1,2,2,3,1,1,2,1,2,3,4))
#   expect_equal(blockmodel_concor(mpn_elite_mex, block.content = "meanrowsum")$membership[1:11], 
#                c(1,1,1,1,1,1,1,1,1,1,1))
#   expect_equal(blockmodel_concor(mpn_elite_mex, block.content = "meancolsum")$membership[1:11], 
#                c(1,1,1,1,1,1,1,1,1,1,1))
#   expect_equal(blockmodel_concor(mpn_elite_mex, block.content = "sum")$membership[1:11], 
#                c(1,1,1,1,1,1,1,1,1,1,1))
#   expect_equal(blockmodel_concor(mpn_elite_mex, block.content = "median")$membership[1:11], 
#                c(1,1,1,1,1,1,1,1,1,1,1))
#   expect_equal(blockmodel_concor(mpn_elite_mex, block.content = "min")$membership[1:11], 
#                c(1,1,1,1,1,1,1,1,1,1,1))
#   expect_equal(blockmodel_concor(mpn_elite_mex, block.content = "max")$membership[1:11], 
#                c(1,1,1,1,1,1,1,1,1,1,1))
#   expect_equal(blockmodel_concor(mpn_elite_mex, block.content = "types")$membership[1:11], 
#                c(1,1,1,1,1,1,1,1,1,1,1))
# })
# 
# test_that("concor works on two-mode networks", {
#   expect_equal(blockmodel_concor(mpn_elite_usa_advice)$membership$nodes1, c(1,2,2,1,1,2,1,1,2,1,2,2,2,2))
#   expect_equal(blockmodel_concor(mpn_elite_usa_advice, p = 2)$membership$nodes1, c(1,3,3,2,2,4,1,1,3,1,3,4,3,3))
#   expect_error(blockmodel_concor(mpn_elite_usa_advice, p = 3)$membership$nodes1, "differing number of rows")
#   expect_equal(blockmodel_concor(mpn_elite_usa_advice)$membership$nodes2, c(1,2,2,1,2,1,2,1,2,1,1,1,2,2,2,1,2,2,1,1))
#   expect_equal(blockmodel_concor(mpn_elite_usa_advice, p = 2)$membership$nodes2, c(1,3,4,2,4,2,4,1,4,1,1,1,4,3,4,1,3,3,2,2))
#   expect_error(blockmodel_concor(mpn_elite_usa_advice, p = 3)$membership$nodes2, "differing number of rows")
# })
# 
# test_that("blockmodel print works correctly", {
#   expect_output(print(blockmodel_concor(mpn_elite_usa_advice, p = 2)), "Network Blockmodel:")
#   expect_output(print(blockmodel_concor(mpn_elite_usa_advice, p = 2)), "First nodeset:")
# })

# 2. Testing blockmodel()

# test_that("concor works on one-mode networks", {
#   # Unimodal
#   # Set things up
#   m182_social <- to_uniplex(ison_algebra, "social_tie")
#   str_res <- cluster_structural_equivalence(ison_algebra)
#   str_clu <- cutree(str_res, 4)
#   test <- blockmodel(m182_social, str_clu)
#   # Test stuff
#   expect_equal(unname(test$block.membership),
#                c(1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,4))
#   expect_equal(unname(test$order.vector),
#                c(1,4,9,10,12,15,2,7,8,13,14,3,5,6,11,16))
#   expect_equal(test$cluster.method, "Prespecified")
#   expect_equal(class(test), "blockmodel")
#   # Bimodal
#   # Set things up
#   str_resbi <- cluster_structural_equivalence(ison_southern_women)
#   str_clubi <- cutree(str_resbi, 4)
#   testbi <- blockmodel(ison_southern_women, str_clubi)
#   # Test stuff
#   expect_equal(unname(testbi$block.membership[[1]]),
#                c(1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2))
#   expect_equal(testbi$modes, 2)
#   expect_equal(length(testbi$plabels), 2)
#   expect_equal(length(testbi$glabels), 32)
#   expect_equal(testbi$cluster.method, "Prespecified")
#   expect_equal(class(testbi), "blockmodel")
# })
# 
# # 3. Testing reduce_graph()
# 
# test_that("concor works on one-mode networks", {
#   # Set things up
#   m182_social <- to_uniplex(ison_algebra, "social_tie")
#   str_res <- cluster_structural_equivalence(ison_algebra)
#   str_clu <- cutree(str_res, 4)
#   block <- blockmodel(m182_social, str_clu)
#   test <- reduce_graph(blockmodel = block)
#   testlabeled <- reduce_graph(blockmodel = block,
#                               block_labels = c("Tennis",
#                                                "Curling",
#                                                "Nordic Walking",
#                                                "Aqua-cycling"))
#   # Test stuff
#   expect_equal(class(test), "igraph")
#   expect_equal(unname(test[1]), c(2.835, 0.100, 0.225, 1.250))
#   expect_equal(unname(testlabeled[1]), c(2.835, 0.100, 0.225, 1.250))
#   expect_equal(names(test[1]), c("Block 1","Block 2","Block 3",
#                                         "Block 4"))
#   expect_equal(names(testlabeled[1]), c("Tennis","Curling","Nordic Walking",
#                                         "Aqua-cycling"))
# 
# })

# 4. Testing summarise_statistics()

# test_that("concor works on one-mode networks", {
#   # Set things up
#   test <- summarise_statistics(node_degree(mpn_elite_mex),
#                        cutree(cluster_structural_equivalence(mpn_elite_mex), 3))
#   test2 <- summarise_statistics(node_triad_census(mpn_elite_mex),
#                                cutree(cluster_structural_equivalence(mpn_elite_mex), 3))
#   # Test stuff
#   expect_equal(class(test), "numeric")
#   expect_equal(round(unname(test), 2), c(6.22, 6.63, 7.43))
#   expect_equal(class(test2), c("matrix", "array"))
#   expect_equal(round(test2[1], 2), 480)
# })
