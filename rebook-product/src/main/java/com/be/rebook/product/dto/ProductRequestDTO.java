package com.be.rebook.product.dto;

import com.be.rebook.common.dto.PaginationRequestDTO;
import jakarta.validation.constraints.Min;
import lombok.*;

import java.util.List;

public class ProductRequestDTO {

    @Getter
    @Builder
    public static class ProductSaveRequestDTO {
        private String title;

        private String content;

        private int price;

        private String university;

        private String major;
    }

    @Getter
    @Builder
    public static class ProductUpdateRequestDTO {
        private String title;

        private String content;

        private int price;

        private String university;

        private String major;

        private List<String> existingImages;
    }

    @Getter
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductStatusRequestDTO {
        private String status;
    }

    @Getter
    @Setter
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ProductFilterDTO extends PaginationRequestDTO {

        @Builder.Default
        private String university = "";

        @Builder.Default
        private String major = "";

        @Builder.Default
        private String title = "";

        @Builder.Default
        @Min(value = 0, message = "최저가는 0이상이어야 합니다.")
        private Integer minPrice = 0;

        @Builder.Default
        @Min(value = 0, message = "최고가는 0이상이어야 합니다.")
        private Integer maxPrice = Integer.MAX_VALUE;

        @Builder.Default
        private String order = "";
    }
}
