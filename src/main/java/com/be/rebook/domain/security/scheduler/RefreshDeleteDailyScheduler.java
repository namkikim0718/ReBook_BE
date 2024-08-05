package com.be.rebook.domain.security.scheduler;

import com.be.rebook.domain.security.service.ReissueService;
import org.springframework.scheduling.annotation.Scheduled;

public class RefreshDeleteDailyScheduler {
    private final ReissueService reissueService;

    public RefreshDeleteDailyScheduler(ReissueService reissueService){
        this.reissueService = reissueService;
    }

    @Scheduled(cron = "0 0 0 * * ?") // 매일 자정에 실행
    public void deleteOldRefresh() {
        reissueService.deleteRefreshsOlderThanOneDay();
    }
}
