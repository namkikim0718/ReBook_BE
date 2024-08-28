package com.be.rebook.common.dto;

import jakarta.validation.constraints.Min;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PaginationRequestDTO {

    @Min(value = 0, message = "페이지 번호는 0 이상이어야 합니다.")
    private int page = 0;

    @Min(value = 1, message = "페이지 크기는 1 이상이어야 합니다.")
    private int size = 10;

    private String sortOrder = "asc"; // 기본 정렬 순서: 오름차순

    private Long lastId; // 마지막 ID (No-Offset Pagination을 위한 필드)
}
