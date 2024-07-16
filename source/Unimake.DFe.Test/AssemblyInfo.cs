using Xunit;

[assembly: CollectionBehavior(DisableTestParallelization = false)]
[assembly: TestCollectionOrderer("XunitExtensions.Orderers.PriorityOrderer", "XunitExtensions")]
[assembly: TestCaseOrderer("XunitExtensions.Orderers.PriorityOrderer", "XunitExtensions")]